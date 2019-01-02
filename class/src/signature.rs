use crate::{field::BaseType, Jtf};

// `<T>`, `<T extends A>`, `<T extends B1 & B2 & B3>`
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TypeParamater {
    pub ident: Jtf,
    pub bounds: Option<(ObjectType, Option<Box<[ObjectType]>>)>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ObjectType {
    // Something like `Foo<T, U>`
    Class(ClassTypeSignature),
    // Something like `Bar<T>[][]`, `int[]`
    Array(TypeSignature),
    // T
    TypeVariable(Jtf),
}

// Lfoo/bar/baz
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ClassSignature {
    pub type_params: Box<[TypeParamater]>,
    pub extends: ClassTypeSignature,
    pub implements: ClassTypeSignature,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ClassTypeSignature {
    pub package: Option<Jtf>,
    pub class: SimpleClassTypeSignature,
    pub inner: Box<[SimpleClassTypeSignature]>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct SimpleClassTypeSignature {
    pub ident: Jtf,
    pub type_arguments: Box<[TypeArgument]>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeBound {
    /// Upper bound
    Extends,
    /// Lower bound
    Super,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeArgument {
    // <*>
    Wildcard,
    // <? extends B> or <? super B>
    Bounded(TypeBound, Box<ObjectType>),
    // <T>
    Normal(Box<ObjectType>),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeSignature {
    Base(BaseType),
    Object(ObjectType),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ArrayTypeSignature {}
