use std::path::PathBuf;
use std::str::FromStr;

use dashmap::DashMap;
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
}

// pub const LEGEND_TYPE: &[SemanticTokenType] = &[
//     SemanticTokenType::FUNCTION,
//     SemanticTokenType::VARIABLE,
//     SemanticTokenType::STRING,
//     SemanticTokenType::COMMENT,
//     SemanticTokenType::NUMBER,
//     SemanticTokenType::KEYWORD,
//     SemanticTokenType::OPERATOR,
//     SemanticTokenType::PARAMETER,
// ];

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                // hover_provider: Some(HoverProviderCapability::Simple(true)),
                // completion_provider: Some(CompletionOptions::default()),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                // semantic_tokens_provider: Some(
                //     SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                //         SemanticTokensRegistrationOptions {
                //             text_document_registration_options: {
                //                 TextDocumentRegistrationOptions {
                //                     document_selector: Some(vec![DocumentFilter {
                //                         language: Some("pancake".into()),
                //                         scheme: Some("file".into()),
                //                         pattern: None,
                //                     }]),
                //                 }
                //             },
                //             semantic_tokens_options: SemanticTokensOptions {
                //                 work_done_progress_options: WorkDoneProgressOptions::default(),
                //                 legend: SemanticTokensLegend {
                //                     token_types: LEGEND_TYPE.into(),
                //                     token_modifiers: vec![],
                //                 },
                //                 range: Some(true),
                //                 full: Some(SemanticTokensFullOptions::Bool(true)),
                //             },
                //             static_registration_options: StaticRegistrationOptions::default(),
                //         },
                //     ),
                // ),
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
        // self.client
        //     .log_message(MessageType::INFO, "File opened!")
        //     .await;
        event!(Level::DEBUG, "opened file");
        let _ = self
            .on_change((params.text_document.text, params.text_document.uri))
            .await;
    }

    #[instrument]
    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        // self.client
        //     .log_message(
        //         MessageType::INFO,
        //         format!("changed file: {:?}", params.content_changes),
        //     )
        //     .await;
        event!(Level::DEBUG, "changed file");
        let _ = self
            .on_change((
                std::mem::take(&mut params.content_changes[0].text),
                params.text_document.uri,
            ))
            .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!("saved file: {}", params.text.unwrap_or("nada".into())),
            )
            .await;
    }

    // async fn semantic_tokens_full(
    //     &self,
    //     params: SemanticTokensParams,
    // ) -> Result<Option<SemanticTokensResult>> {
    //     let uri = params.text_document.uri.to_string();
    //     self.client
    //         .log_message(MessageType::INFO, "semantic_token_full")
    //         .await;
    //     let semantic_tokens = || -> Option<Vec<SemanticToken>> {
    //         let mut im_complete_tokens = self.semantic_token_map.get_mut(&uri)?;
    //     };
    //     todo!()
    // }
}

impl Backend {
    async fn on_change(&self, params: (String, Url)) -> anyhow::Result<()> {
        let rope = Rope::from_str(&params.0);
        self.file_map.insert(params.1.to_string(), rope);
        // self.client
        //     .log_message(MessageType::INFO, "this could fail")
        //     .await;
        let asts = get_sexprs(params.0)
            .unwrap()
            .iter()
            .map(|s| SExprParser::parse_sexpr(s))
            .collect::<anyhow::Result<Vec<_>>>()
            .unwrap();
        // self.client
        //     .log_message(MessageType::INFO, "it didn't")
        //     .await;
        // for ast in &asts {
        //     self.client
        //         .log_message(MessageType::INFO, format!("Parsed AST:\n{:?}", &ast.body))
        //         .await;
        // }
        self.create_vpr_file(params.1.clone(), &asts).await;
        self.asts_map.insert(params.1.into(), asts);
        Ok(())
    }

    async fn create_vpr_file(&self, uri: Url, asts: &[PancakeFnDec]) {
        let mut path = PathBuf::from_str(uri.path()).unwrap();
        path.set_extension("vpr");
        // let vpr_uri = Url::from_file_path(vpr_path.).unwrap();
        let vpr_uri = Url::from_file_path(path).unwrap();

        let text_edit = TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 0,
                },
            },
            new_text: format!("{:?}", asts[0]),
        };
        self.client
            .log_message(MessageType::INFO, format!("Ziopear {:?}", vpr_uri))
            .await;
        let text_document_id = OptionalVersionedTextDocumentIdentifier {
            uri: vpr_uri,
            version: None,
        };

        let edits = vec![OneOf::Left(text_edit)];

        // Create the `TextDocumentEdit`
        let text_document_edit = TextDocumentEdit {
            text_document: text_document_id,
            edits,
        };

        // Create the `WorkspaceEdit`
        let workspace_edit = WorkspaceEdit {
            changes: None,
            document_changes: Some(DocumentChanges::Edits(vec![text_document_edit])),
            change_annotations: None,
        };
        self.client.apply_edit(workspace_edit).await;
    }
}

#[tokio::main]
async fn main() {
    println!("Starting Pancake Language Server...");
    // let file_appender = tracing_appender::rolling::never("", "lsp.log");
    // tracing_subscriber::fmt().with_writer(file_appender).init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        file_map: DashMap::new(),
        asts_map: DashMap::new(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
