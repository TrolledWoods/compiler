# Compiler

A compiler for some kind of strange
language written in rust.

Is going to compile to a special byte code for a stack based
VM when it's done, but it's not even close to done yet.

# Language syntax 
```
// Comments are defined with '//'

// Types are defined like this:
type Vec = {
    x: f32,
	y: f32,

	// Unfinished:
	// Aliasing values in a struct, i.e.
	// just replace the value name with the
	// name you are pointing to.
	// Maybe this will have the same/similar syntax
	// to aliasing stack values?
	// z1 => x,
};

// A named type is always unique.
// This type for example is not conceptually a Vec,
// even though it's exactly the same in memory
type Vec2 = Vec;
// These types can however be casted between each other.

// If you just want to make a new name for a type,
// and not define a new type, use "alias"
alias TheSameVec = Vec;

// Most other things are just constants. Constants
// are just values that have a type and a location in
// the "read-only" portion of the finished program.
// These values cannot be modified, but can be accessed
// at any point in the program. Also, these types
// may be inlined, so that they don't even have
// to be indirectly accessed at runtime.

// A constant may require you to give it a specific type
// unless the type of the expression to the right is
// extremely clear, like a function, or a cast
const PI : f32 = 3.1415;

// Maybe this is the same?
const ALSO_PI = cast<f32> { 3.1415 };

// This is a function.
// The type parameter for this would be "() -> ()"
const main = [] {
    // "let" is only allowed inside functions.
	// It creates a value on the stack.
	// Not sure about how struct definitions should look
	// yet, we'll see I guess
	let pos = cast<Vec> { x: 50.0, y: 20.0 };

    // We can alias things on the stack too!
	// Aliasing will not support pointer dereferencing
	// though.
	// Here, modifying "x" will be equivalent to modifying
	// pos.x
	alias x = pos.x;
	alias y = pos.y;

	// This may be possible? Not sure yet.
	// This would introduce both x and y
	// into the local scope
	// The syntax of this may change, or it
	// may just not become possible at all
	alias * = pos.*;
};
```
