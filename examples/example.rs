struct Foo;

impl Foo {
    fn one(&self) {}

    fn two(&mut self) -> usize {
        42
    }
}

struct Bar;

impl Bar {
    fn three(&mut self) -> Result<bool, String> {
        Ok(true)
    }

    fn four(&self) -> Vec<i64> {
        vec![1, 2, 3]
    }
}
