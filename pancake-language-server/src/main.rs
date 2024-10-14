use std::cell::RefCell;
use std::env;
use std::path::PathBuf;
use std::str::FromStr;

use anyhow::anyhow;
use dashmap::DashMap;
use expanduser::expanduser;
use notification::ShowMessage;
use pancake2viper::ir;
use pancake2viper::utils::{EncodeOptions, ProgramToViper, ViperHandle};

use serde_json::Value;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use tracing::{event, Level};

// #[derive(Debug)]
struct Backend {
    viper: Mutex<ViperHandle>,
    client: Client,
    file_map: DashMap<String, ir::Program>,
    cake_path: String,
    current_file: Mutex<RefCell<Option<Url>>>,
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
        let update = self
            .update_ast(&params.text_document.uri, params.text_document.text)
            .await;
        if update.is_ok() {
            let _ = self.transpile_file(params.text_document.uri).await;
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let _ = self
            .update_ast(
                &params.text_document.uri,
                params.content_changes[0].text.to_owned(),
            )
            .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        self.current_file.lock().await.replace(Some(uri));
        let _ = self.transpile_file(params.text_document.uri).await;
    }
}

impl Backend {
    async fn update_ast(&self, uri: &Url, program: String) -> anyhow::Result<()> {
        self.current_file.lock().await.replace(Some(uri.clone()));
        let program = pancake2viper::pancake::Program::parse_str(program, &self.cake_path)?;
        let program: ir::Program = program.try_into()?;
        self.file_map.insert(uri.to_string(), program);
        Ok(())
    }

    async fn get_current_ast(&self) -> ir::Program {
        self.file_map
            .get(
                self.current_file
                    .lock()
                    .await
                    .borrow()
                    .as_ref()
                    .unwrap()
                    .as_str(),
            )
            .unwrap()
            .clone()
    }

    async fn transpile_file(&self, uri: Url) -> anyhow::Result<()> {
        let program = self.file_map.get(uri.as_str()).unwrap().clone();
        let viper = self.viper.lock().await;
        let program = program.to_viper(viper.ast, EncodeOptions::default())?;

        self.create_vpr_file(uri, viper.pretty_print(program)).await;
        Ok(())
    }

    async fn create_empty_file(&self, uri: Url) {
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

    async fn verify_command(&self) -> Result<Option<Value>> {
        let mut viper = self.viper.lock().await;
        let program = self
            .get_current_ast()
            .await
            .to_viper(viper.ast, EncodeOptions::default())
            .unwrap();
        let ver = viper.verify(program);
        let result = serde_json::json!({
            "message": ver,
        });
        self.client
            .send_notification::<ShowMessage>(ShowMessageParams {
                typ: MessageType::INFO,
                message: ver,
            })
            .await;
        Ok(Some(result))
    }
}

fn expand_home(s: &str) -> String {
    expanduser(s).unwrap().to_str().unwrap().to_owned()
}

#[tokio::main]
async fn main() {
    let cake_path = expand_home(&env::var("CAKE_ML").unwrap_or("cake".into()));
    let viper_home = expand_home(&env::var("VIPER_HOME").unwrap());
    let z3 = expand_home(&env::var("Z3_EXE").unwrap());

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let viper = Mutex::new(ViperHandle::new(viper_home, z3));

    let (service, socket) = LspService::build(|client| Backend {
        viper,
        client,
        file_map: DashMap::new(),
        cake_path,
        current_file: Mutex::new(RefCell::new(None)),
    })
    .custom_method("custom/pancakeVerify", Backend::verify_command)
    .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}
