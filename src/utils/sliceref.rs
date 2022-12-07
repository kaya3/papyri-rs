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

impl <S, T> From<S> for SliceRef<T> where S: Into<Rc<[T]>> {
    fn from(original: S) -> SliceRef<T> {
        let original = original.into();
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
    
    /// Indicates whether this slice is empty.
    pub fn is_empty(&self) -> bool {
        self.a == self.b
    }
    
    /// Returns the length of this slice.
    pub fn len(&self) -> usize {
        self.b - self.a
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
}

impl <T: PartialEq> PartialEq for SliceRef<T> {
    fn eq(&self, other: &SliceRef<T>) -> bool {
        self.as_ref() == other.as_ref()
    }
}
impl <T: Eq> Eq for SliceRef<T> {}
