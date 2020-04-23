pub const DO_DEBUGGING: bool = true;

macro_rules! err {
	( $parser:expr, $error:expr ) => {{
		use crate::debugging::*;

		if DO_DEBUGGING {
			println!("{:?}", $parser.call_stack);
			println!("Error at '{}' line {}", file!(), line!());
			panic!("{:?}", $error);
			}

		Err($error.into())
		}};
}
