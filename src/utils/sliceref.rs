use std::rc::Rc;

use crate::errors;

#[derive(Debug, Clone)]
/// A reference-counted pointer to a slice, allowing reference-counted pointers
/// to sub-slices without copying.
pub struct SliceRef<T> {
    original: Rc<[T]>,
    a: u32,
    b: u32,
}

impl <S, T> From<S> for SliceRef<T> where Rc<[T]>: From<S> {
    fn from(original: S) -> SliceRef<T> {
        let original = Rc::from(original);
        let b = original.len() as u32;
        SliceRef {original, a: 0, b}
    }
}

impl <T> AsRef<[T]> for SliceRef<T> {
    fn as_ref(&self) -> &[T] {
        &self.original[self.a as usize..self.b as usize]
    }
}

impl <T> SliceRef<T> {
    /// Returns a reference to the element at index `i` in this slice.
    pub(crate) fn get(&self, i: usize) -> &T {
        &self.original[self.a as usize + i]
    }
    
    /// Indicates whether this slice is empty.
    pub(crate) fn is_empty(&self) -> bool {
        self.a == self.b
    }
    
    /// Returns the length of this slice.
    pub(crate) fn len(&self) -> usize {
        (self.b - self.a) as usize
    }
    
    /// Creates a new reference-counted pointer to a sub-slice of this slice,
    /// without copying.
    pub(crate) fn slice(&self, a: usize, b: usize) -> SliceRef<T> {
        if a > b || self.a as usize + b > self.original.len() { errors::ice("illegal slice"); }
        
        SliceRef {
            original: self.original.clone(),
            a: self.a + a as u32,
            b: self.a + b as u32,
        }
    }
}

impl <T: PartialEq> PartialEq for SliceRef<T> {
    fn eq(&self, other: &SliceRef<T>) -> bool {
        self.as_ref() == other.as_ref()
    }
}
impl <T: Eq> Eq for SliceRef<T> {}
