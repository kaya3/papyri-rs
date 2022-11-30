use std::rc::Rc;

use crate::errors;

#[derive(Debug, Clone)]
/// A reference-counted pointer to a slice, allowing reference-counted pointers
/// to sub-slices without copying.
pub struct SliceRef<T> {
    original: Rc<[T]>,
    a: usize,
    b: usize,
}

impl <T> From<Vec<T>> for SliceRef<T> {
    fn from(original: Vec<T>) -> SliceRef<T> {
        SliceRef::from(Rc::from(original))
    }
}

impl <T> From<Rc<[T]>> for SliceRef<T> {
    fn from(original: Rc<[T]>) -> SliceRef<T> {
        let b = original.len();
        SliceRef {original, a: 0, b}
    }
}

impl <T> AsRef<[T]> for SliceRef<T> {
    fn as_ref(&self) -> &[T] {
        &self.original[self.a..self.b]
    }
}

impl <T> SliceRef<T> {
    /// Returns a reference to the element at index `i` in this slice.
    pub fn get(&self, i: usize) -> &T {
        &self.original[self.a + i]
    }
    
    /// Creates a new reference-counted pointer to a sub-slice of this slice,
    /// without copying.
    pub fn slice(&self, a: usize, b: usize) -> SliceRef<T> {
        if a > b || self.a + b > self.original.len() { errors::ice("illegal slice"); }
        
        SliceRef {
            original: self.original.clone(),
            a: self.a + a,
            b: self.a + b,
        }
    }
    
    /// Returns the length of this slice.
    pub fn len(&self) -> usize {
        self.b - self.a
    }
}
