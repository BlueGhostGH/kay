use std::{fmt, ops};

use crate::span::Span;

#[derive(Clone, PartialEq)]
pub struct Node<T, M> {
    inner: Box<T>,
    meta: M,
}

impl<T, M> Node<T, M> {
    pub fn new(inner: T, meta: M) -> Self {
        Self {
            inner: Box::new(inner),
            meta,
        }
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    pub fn into_inner(self) -> T {
        *self.inner
    }

    pub fn map<U, F>(self, f: F) -> Node<U, M>
    where
        F: FnOnce(T) -> U,
    {
        Node {
            inner: Box::new(f(*self.inner)),
            meta: self.meta,
        }
    }

    pub fn meta(&self) -> &M {
        &self.meta
    }

    pub fn meta_mut(&mut self) -> &mut M {
        &mut self.meta
    }
}

impl<T, M> ops::Deref for Node<T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

impl<T, M> ops::DerefMut for Node<T, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner_mut()
    }
}

impl<T: fmt::Debug, M: fmt::Debug> fmt::Debug for Node<T, M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?} @ {:?}", self.inner, self.meta)
    }
}

pub type SrcNode<T> = Node<T, Span>;

impl<T> SrcNode<T> {
    pub fn span(&self) -> Span {
        self.meta.clone()
    }
}
