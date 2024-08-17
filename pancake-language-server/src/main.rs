use std::path::PathBuf;
use std::str::FromStr;

use dashmap::DashMap;
use pancake2viper::translation::translate_fndec;
use ropey::Rope;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use tracing::{event, instrument, Level};

use pancake2viper::pancake_ast::PancakeFnDec;
use pancake2viper::parser::{get_sexprs, SExprParser};

#[derive(Debug)]
struct Backend {
    client: Client,
    file_map: DashMap<String, Rope>,
    asts_map: DashMap<String, Vec<PancakeFnDec>>,
    cake_path: String,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..ServerCapabilities::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    #[instrument]
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let _ = self
            .on_change((params.text_document.text, params.text_document.uri))
            .await;
    }

    #[instrument]
    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        let _ = self
            .on_change((
                std::mem::take(&mut params.content_changes[0].text),
                params.text_document.uri,
            ))
            .await;
    }

    async fn did_save(&self, _params: DidSaveTextDocumentParams) {}
}

impl Backend {
    async fn on_change(&self, params: (String, Url)) -> anyhow::Result<()> {
        let rope = Rope::from_str(&params.0);
        self.file_map.insert(params.1.to_string(), rope);
        let asts = get_sexprs(params.0, &self.cake_path)?
            .iter()
            .map(|s| SExprParser::parse_sexpr(s))
            .collect::<anyhow::Result<Vec<_>>>()?;

        self.create_vpr_file(params.1.clone(), &asts).await;
        self.asts_map.insert(params.1.into(), asts);
        Ok(())
    }

    async fn create_empty_file(&self, uri: Url) {
        event!(Level::DEBUG, "creating empty file {}", uri.to_string());
        let create_file = CreateFile {
            uri,
            options: None,
            annotation_id: None,
        };
        let doc_change = DocumentChangeOperation::Op(ResourceOp::Create(create_file));
        let workspace_edit = WorkspaceEdit {
            change_annotations: None,
            changes: None,
            document_changes: Some(DocumentChanges::Operations(vec![doc_change])),
        };
        self.client.apply_edit(workspace_edit).await.unwrap();
    }

    async fn create_vpr_file(&self, uri: Url, asts: &[PancakeFnDec]) {
        event!(Level::DEBUG, "creating viper file {}", uri.to_string());
        // Create empty .vpr file
        let mut path = PathBuf::from_str(uri.path()).unwrap();
        path.set_extension("vpr");
        let vpr_uri = Url::from_file_path(path).unwrap();
        self.create_empty_file(vpr_uri.clone()).await;

        // Compile the AST into Viper code
        let vpr_code = asts
            .iter()
            .map(|e| translate_fndec(e).to_string())
            .collect::<Vec<_>>()
            .join("\n");

        // Insert the Viper code into the .vpr file
        let text_edit = TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: u32::MAX,
                    character: u32::MAX,
                },
            },
            new_text: vpr_code,
        };

        let text_document_edit = TextDocumentEdit {
            text_document: OptionalVersionedTextDocumentIdentifier {
                uri: vpr_uri.clone(),
                version: None,
            },
            edits: vec![OneOf::Left(text_edit)],
        };

        let workspace_edit = WorkspaceEdit {
            changes: None,
            document_changes: Some(DocumentChanges::Edits(vec![text_document_edit])),
            change_annotations: None,
        };

        self.client.apply_edit(workspace_edit).await.unwrap();
    }
}

#[tokio::main]
async fn main() {
    let file_appender = tracing_appender::rolling::never(".", "lsp.log");
    tracing_subscriber::fmt().with_writer(file_appender).init();

    let cake_path = std::env::var("CAKE_ML").unwrap_or("cake".into());

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        file_map: DashMap::new(),
        asts_map: DashMap::new(),
        cake_path,
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
