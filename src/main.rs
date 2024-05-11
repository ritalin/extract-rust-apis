use tracing_subscriber::{filter, prelude::*};

fn main() {
    let stdout_log = tracing_subscriber::fmt::layer().compact()
        .without_time()
        .with_ansi(false)
        .with_target(false)
    ;
    tracing_subscriber::registry()
    .with(stdout_log.with_filter(filter::LevelFilter::TRACE))
    .init();

    for crate_name in &vec!["std", "alloc", "core"] {
        fndump::compile::run(
            crate_name, 
            fndump::core::ProcessHandler{}, 
            fndump::print::PrintHandler::new(crate_name, "exp", true)
        );
    }
}