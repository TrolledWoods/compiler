pub const DO_DEBUGGING: bool = true;
pub const DO_PANIC_ON_ERROR: bool = false;

macro_rules! debug_err {
	( $error:expr ) => {{
		use crate::debugging::*;

		if DO_DEBUGGING {
			println!("Error at '{}' line {}", file!(), line!());
			}

		if DO_PANIC_ON_ERROR {
			panic!("{:?}", $error);
			}

		Err($error.into())
		}};
}

macro_rules! err {
	( $error:expr ) => {{
		use crate::debugging::*;

		if DO_DEBUGGING {
			println!("Error at '{}' line {}", file!(), line!());
			}

		if DO_PANIC_ON_ERROR {
			panic!("{:?}", $error);
			}

		$error
		}};
}
