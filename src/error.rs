use crate::lexer::SourcePos;
use ansi_term::Color;

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

        for problem in self.main_problems.iter() {
            problem.print(Color::Red);
        }

        for problem in self.added_info.iter() {
            problem.print(Color::Green);
        }

        println!("");
    }
}

#[derive(Clone, Debug)]
struct Message {
    pub pos: SourcePos,
    pub message: String,
}

impl Message {
    fn print(&self, _colour: Color) {
        println!("");
        println!("{}", self.message);

        // Load the file
        let input = std::fs::read_to_string(self.pos.file.to_string());

        let input = match input {
            Ok(val) => val,
            Err(err) => {
                println!("Cannot access source file {}", self.pos.file);
                return;
            },
        };

        if self.pos.start.line == std::usize::MAX {
            let mut last_line = None;
            for (i, line) in input.lines().enumerate() {
                last_line = Some((i, line));
            }
            let last_line = last_line.unwrap_or_else(|| (0, ""));
            let n_digits = get_digits(last_line.0 + 1);

            println!("{}: ", self.pos.file);
            print_code_line(last_line.0 + 1, last_line.1, false, n_digits);

            return;
        }

        let start_line = self.pos.start.line;
        let start_char = self.pos.start.character;
        let padded_start_line = if start_line > 0 { start_line - 1 } else { 0 };
        let end_line = self.pos.end.line;
        let end_char = self.pos.end.character;
        let padded_end_line = end_line + 1;

        let n_digits = get_digits(padded_end_line + 1);

        let mut lines = input.lines().skip(padded_start_line);

        if start_line == end_line {
            for line_index in padded_start_line..=padded_end_line {
                let line = match lines.next() {
                    Some(line) => line,
                    None => panic!("Invalid line in error message"),
                };

                print_code_line(line_index + 1, line, false, n_digits);

                // Mark the location
                if line_index == start_line {
                    print!("{:1$}   ", "", n_digits);
                    print!("{:1$}", "", start_char);
                    for _ in start_char..end_char {
                        print!("^");
                    }

                    println!("-- HERE");
                }
            }
        } else {
            for line_index in padded_start_line..=padded_end_line {
                let line = match lines.next() {
                    Some(line) => line,
                    None => panic!("Invalid line in error message"),
                };

                if line_index == start_line {
                    print!("{:1$}   ", "", n_digits);
                    print!("{:1$}", "", start_char);
                    for _ in start_char..end_char {
                        print!("v");
                    }
                    for _ in end_char..line.len() {
                        print!("-");
                    }
                    println!("");
                }

                print_code_line(line_index + 1, line, false, n_digits);

                if line_index == start_line {
                    print!("{:1$}   ", "", n_digits);
                    for _ in 0..start_char {
                        print!("-");
                    }
                    for _ in start_char..end_char {
                        print!("^");
                    }
                    println!("");
                }
            }
        }
    }
}

fn print_code_line(line_num: usize, line: &str, use_ansi: bool, num_width: usize) {
    println!("{:>width$} | {}", line_num, line, width = num_width);
}

fn get_digits(mut num: usize) -> usize {
    let mut n_digits = 1;

    while num >= 10 {
        num /= 10;
        n_digits += 1;
    }

    n_digits
}
