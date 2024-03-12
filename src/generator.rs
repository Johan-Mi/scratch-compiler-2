#[derive(Default)]
pub struct Generator {
    counter: u16,
}

impl Generator {
    pub fn new_u16(&mut self) -> u16 {
        let n = self.counter;
        self.counter += 1;
        n
    }
}
