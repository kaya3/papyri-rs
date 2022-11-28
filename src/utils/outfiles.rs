use std::path::Path;

pub struct OutFiles<T> {
    base_path: Option<Box<Path>>,
    v: Vec<(Box<Path>, T)>,
}

impl <T> OutFiles<T> {
    pub fn new(base_path: Option<Box<Path>>) -> OutFiles<T> {
        OutFiles {
            base_path,
            v: Vec::new(),
        }
    }
    
    pub fn is_empty(&self) -> bool {
        self.v.is_empty()
    }
    
    pub fn push<P: AsRef<Path>>(&mut self, path: P, t: T) {
        let p = self.base_path.as_ref()
            .map_or_else(
                || Box::from(path.as_ref()),
                |base| base.join(path.as_ref()).into_boxed_path(),
            );
        self.v.push((p, t));
    }
    
    pub fn clear(&mut self) {
        self.v.clear();
    }
    
    pub fn iter<'a>(&'a self) -> impl Iterator<Item=(&Path, &T)> {
        self.v.iter()
            .map(|(p, t)| (p.as_ref(), t))
    }
}
