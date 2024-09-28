use std::path::PathBuf;
use std::str::FromStr;

use dashmap::DashMap;
use pancake2viper::utils::pretty_print;

use pancake2viper::Viper;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use tracing::{event, Level};

// #[derive(Debug)]
struct Backend {
    viper: Viper,
    client: Client,
    file_map: DashMap<String, String>,
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

    // #[instrument]
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.file_map.insert(
            params.text_document.uri.to_string(),
            params.text_document.text,
        );
        let _ = self.verify_file(params.text_document.uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.file_map.insert(
            params.text_document.uri.to_string(),
            params.content_changes[0].text.to_owned(),
        );
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.verify_file(params.text_document.uri).await.unwrap();
    }
}

impl Backend {
    async fn verify_file(&self, uri: Url) -> anyhow::Result<()> {
        let file_contents = self.file_map.get(uri.as_str()).unwrap().clone();
        let program = pancake2viper::pancake::Program::parse_str(file_contents, &self.cake_path)?;

        self.create_vpr_file(uri, pretty_print(&self.viper, program.clone())?)
            .await;
        Ok(())
    }

    async fn create_empty_file(&self, uri: Url) {
        // event!(Level::DEBUG, "creating empty file {}", uri.to_string());
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

    async fn create_vpr_file(&self, uri: Url, text: String) {
        event!(Level::DEBUG, "creating viper file {}", uri.to_string());
        // Create empty .vpr file
        let mut path = PathBuf::from_str(uri.path()).unwrap();
        path.set_extension("vpr");
        let vpr_uri = Url::from_file_path(path).unwrap();
        self.create_empty_file(vpr_uri.clone()).await;

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
            new_text: text,
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

    let viper_home = std::env::var("VIPER_HOME").unwrap();
    let viper = Viper::new(&viper_home);

    let (service, socket) = LspService::new(|client| Backend {
        viper,
        client,
        file_map: DashMap::new(),
        cake_path,
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
