use clap::{ArgAction, Parser};

#[derive(Parser, Debug)]
#[command(author, version, about)]
pub struct Cli {
    /// trade `level synchronization` to achieve maximum speed possible...
    #[arg(short = 'r', long = "rush", action = ArgAction::SetTrue)]
    pub rush: bool,

    /// print default job
    #[arg(short = 'd', long = "default-job", action = ArgAction::SetTrue)]
    pub print_default_job: bool,

    /// list all jobs and exit
    #[arg(long = "list-jobs", action = ArgAction::SetTrue)]
    pub list_jobs: bool,

    /// list all rules and exit
    #[arg(long = "list-rules", action = ArgAction::SetTrue)]
    pub list_rules: bool,

    /// list all jobs and rules and exit
    #[arg(short = 'l', long = "list", action = ArgAction::SetTrue)]
    pub list_jobs_and_rules: bool,

    /// check if up-to-date, without running anything
    #[arg(short = 'u', long = "up-to-date", action = ArgAction::SetTrue)]
    pub check_is_up_to_date: bool,

    /// only print commands, without running anything
    #[arg(short = 'p', long = "print-commands", action = ArgAction::SetTrue)]
    pub print_commands: bool,

    /// print commands only if stderr is not empty
    #[arg(short = 'q', long = "quiet", action = ArgAction::SetTrue)]
    pub quiet: bool,

    /// print both command and description while executing job
    #[arg(short = 'v', long = "verbose", action = ArgAction::SetTrue)]
    pub verbose: bool,

    /// always build job, no matter if it is up-to-date or not
    #[arg(short = 'B', long = "always-build", action = ArgAction::SetTrue)]
    pub always_build: bool,

    /// cd into directory before doing anything
    #[arg(short = 'f', long = "file")]
    pub file_path: Option<String>,

    /// specify file path to `rush` script
    #[arg(long = "change-dir")]
    pub change_dir: Option<String>,

    /// specify default target
    #[arg(short = 't', long = "target")]
    pub default_target: Option<String>,

    /// specify count of fails until exit
    #[arg(short = 'k', long = "keep-going", default_value_t = 1)]
    pub max_fail_count: usize,
}
