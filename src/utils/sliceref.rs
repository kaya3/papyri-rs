use std::rc::Rc;

use super::diagnostics::ice;

#[derive(Debug, Clone)]
pub struct SliceRef<T> {
    original: Rc<[T]>,
    a: usize,
    b: usize,
}

impl <T> From<Vec<T>> for SliceRef<T> {
    fn from(original: Vec<T>) -> Self {
        Self::from(Rc::from(original))
    }
}

impl <T> From<Rc<[T]>> for SliceRef<T> {
    fn from(original: Rc<[T]>) -> Self {
        let b = original.len();
        SliceRef {original, a: 0, b}
    }
}

impl <T> SliceRef<T> {
    pub fn slice(&self, a: usize, b: usize) -> SliceRef<T> {
        if a > b || self.a + b > self.original.len() { ice("illegal slice"); }
        
        SliceRef {
            original: self.original.clone(),
            a: self.a + a,
            b: self.a + b,
        }
    }
    
    pub fn as_ref(&self) -> &[T] {
        &self.original[self.a..self.b]
    }
    
    pub fn len(&self) -> usize {
        self.b - self.a
    }
}
