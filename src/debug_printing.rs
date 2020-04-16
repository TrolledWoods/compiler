use crate::string_pile::TinyString;
use std::cmp::{Ord, Ordering, PartialOrd};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LogPosition {
    Above,
    Below,
}

impl PartialOrd for LogPosition {
    fn partial_cmp(&self, other: &LogPosition) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LogPosition {
    fn cmp(&self, other: &LogPosition) -> Ordering {
        use LogPosition::*;
        match self {
            Above => match other {
                Above => Ordering::Equal,
                Below => Ordering::Greater,
            },
            Below => match other {
                Above => Ordering::Less,
                Below => Ordering::Equal,
            },
        }
    }
}

pub struct FeedbackLog<'a> {
    pub file: TinyString,
    pub line: usize,
    pub start_char: Option<usize>,
    pub end_char: Option<usize>,
    pub position: LogPosition,
    pub message: &'a str,
}

pub fn pretty_print_feedback(feedback_logs: Vec<FeedbackLog<'_>>) {
    use std::collections::HashMap;

    const N_PADDING_LINES: usize = 1;

    let mut file_grouped_logs: HashMap<_, Vec<FeedbackLog>> = HashMap::new();
    let mut n_digits = 0;
    for log in feedback_logs {
        let log_n_digits = get_digits(log.line + N_PADDING_LINES + 1);
        if log_n_digits > n_digits {
            n_digits = log_n_digits;
        }

        match file_grouped_logs.get_mut(&log.file) {
            Some(logs) => logs.push(log),
            None => {
                file_grouped_logs.insert(log.file, vec![log]);
            }
        }
    }

    // Print each file
    for (file, mut logs) in file_grouped_logs {
        println!("'{}': \n", file);
        let file_content = match std::fs::read_to_string(file.to_string()) {
            Ok(content) => content,
            Err(_) => {
                println!(" --- File doesn't exist ");
                continue;
            }
        };

        // Really complicated sorting order because
        // the order of things isn't always obvious
        logs.sort_by(|a, b| match a.line.cmp(&b.line) {
            Ordering::Equal => match a.position.cmp(&b.position) {
                Ordering::Equal => match a.position {
                    LogPosition::Above => a.start_char.unwrap_or(0).cmp(&b.start_char.unwrap_or(0)),
                    LogPosition::Below => b.start_char.unwrap_or(0).cmp(&a.start_char.unwrap_or(0)),
                },
                c => c,
            },
            c => c,
        });

        let mut last_log_line = None;
        let mut logs = logs.into_iter();
        let mut next_log = logs.next();

        for (line_counter, line) in file_content.lines().enumerate() {
            if let Some(log) = &next_log {
                let wanted_next_line = match log.position {
                    LogPosition::Above => log.line,
                    LogPosition::Below => log.line + 1,
                };

                if wanted_next_line == line_counter {
                    print_message(
                        n_digits,
                        log.message,
                        log.position,
                        log.start_char,
                        log.end_char,
                    );

                    last_log_line = Some(log.line);
                    next_log = logs.next();
                }
            }

            if let Some(next_log) = &next_log {
                if line_counter + N_PADDING_LINES >= next_log.line {
                    print_line(line_counter, n_digits, line);
                }
            }

            if let Some(last_log_line) = last_log_line {
                if line_counter <= last_log_line + N_PADDING_LINES {
                    print_line(line_counter, n_digits, line);
                }

                if line_counter == last_log_line + N_PADDING_LINES + 1 {
                    println!("{:>1$}... ", "", n_digits);
                }
            }
        }

        // Print any remaining logs that were at the
        // end of the file
        if let Some(log) = next_log {
            print_message(
                n_digits,
                log.message,
                log.position,
                log.start_char,
                log.end_char,
            );

            for log in logs {
                print_message(
                    n_digits,
                    log.message,
                    log.position,
                    log.start_char,
                    log.end_char,
                );
            }
        }
    }
}

fn print_message(
    n_digits: usize,
    message: &str,
    pos: LogPosition,
    start: Option<usize>,
    end: Option<usize>,
) {
    fn print_arrow(arrow: char, line: &str, start: Option<usize>, end: Option<usize>) {
        match (start, end) {
            (Some(start), Some(end)) => {
                print!("{:>1$}", "", start);
                (start..end).for_each(|_c| print!("{}", arrow));
            }
            (None, Some(end)) => {
                print!("{:->1$}", "", end);
                print!("{}", arrow);
            }
            (Some(start), None) => {
                print!("{:>1$}", "", start);
                print!("{}", arrow);
                print!("{:->1$}", "", line.len() - start);
            }
            (None, None) => {
                print!("{:->1$}", "", line.len());
            }
        }
    }

    // Yucky printing code for messages
    match pos {
        LogPosition::Above => {
            println!(
                "{:>1$}   {2}",
                "",
                n_digits + start.unwrap_or(end.unwrap_or(0)),
                message
            );

            print!("{:>1$}   ", "", n_digits);
            print_arrow('v', message, start, end);
            println!("");
        }
        LogPosition::Below => {
            print!("{:>1$}   ", "", n_digits);
            print_arrow('^', message, start, end);
            println!("");

            println!(
                "{:>1$}   {2}",
                "",
                n_digits + start.unwrap_or(end.unwrap_or(0)),
                message
            );
        }
    };
}

fn print_line(line_number: usize, n_digits: usize, line: &str) {
    println!("{:>1$} | {2}", line_number + 1, n_digits, line);
}

fn get_digits(mut num: usize) -> usize {
    let mut digits = 1;

    while num >= 10 {
        num /= 10;
        digits += 1;
    }

    digits
}
