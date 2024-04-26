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

    fndump::compile::run(
        "std", 
        fndump::core::ProcessHandler{}, 
        fndump::print::PrintHandler::new("std", "exp", true)
    );
}