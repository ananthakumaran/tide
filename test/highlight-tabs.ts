type Fnord = number;

function foo(a: Fnord, b: Fnord): Fnord {
	while (a + b) {
		a = a - b;
	}
	return a + b;
}

const moo: Fnord = 1;

foo();

export {};
