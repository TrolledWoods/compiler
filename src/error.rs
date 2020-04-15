use crate::debug_printing::{pretty_print_feedback, FeedbackLog, LogPosition};
use crate::lexer::SourcePos;

pub trait CompileError {
    fn get_printing_data(self) -> ErrorPrintingData;
}

#[derive(Clone, Debug)]
pub struct ErrorPrintingData {
    error: String,
    main_problems: Vec<Message>,
    added_info: Vec<Message>,
}

impl ErrorPrintingData {
    pub fn new(error: String) -> ErrorPrintingData {
        ErrorPrintingData {
            error,
            main_problems: Vec::new(),
            added_info: Vec::new(),
        }
    }

    pub fn problem(mut self, pos: SourcePos, message: String) -> ErrorPrintingData {
        self.main_problems.push(Message { pos, message });
        self
    }

    pub fn info(mut self, pos: SourcePos, message: String) -> ErrorPrintingData {
        self.added_info.push(Message { pos, message });
        self
    }

    pub fn print(&self) {
        println!("ERROR: {}", self.error);

        let main_problems = self.main_problems.iter();
        let mut logs = Vec::new();

        for problem in main_problems {
            if problem.pos.start.line == problem.pos.end.line {
                logs.push(FeedbackLog {
                    file: problem.pos.file,
                    line: problem.pos.start.line,
                    start_char: Some(problem.pos.start.character),
                    end_char: Some(problem.pos.end.character),
                    position: LogPosition::Below,
                    message: &problem.message,
                });
            } else {
                logs.push(FeedbackLog {
                    file: problem.pos.file,
                    line: problem.pos.start.line,
                    start_char: Some(problem.pos.start.character),
                    end_char: None,
                    position: LogPosition::Above,
                    message: "Range start",
                });
                logs.push(FeedbackLog {
                    file: problem.pos.file,
                    line: problem.pos.end.line,
                    start_char: None,
                    end_char: Some(problem.pos.end.character),
                    position: LogPosition::Below,
                    message: &problem.message,
                });
            }
        }

        pretty_print_feedback(logs);

        println!("");
    }
}

#[derive(Clone, Debug)]
struct Message {
    pub pos: SourcePos,
    pub message: String,
}
