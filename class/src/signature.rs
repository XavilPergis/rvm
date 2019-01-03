use crate::field::BaseType;

// `<T>`, `<T extends A>`, `<T extends B1 & B2 & B3>`
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TypeParamater {
    pub ident: String,
    pub bounds: Option<(ObjectType, Option<Box<[ObjectType]>>)>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ObjectType {
    // Something like `Foo<T, U>`
    Class(Box<ClassTypeSignature>),
    // Something like `Bar<T>[][]`, `int[]`
    Array(usize, Box<TypeSignature>),
    // T
    TypeVariable(String),
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
    pub package: Option<String>,
    pub class: SimpleClassTypeSignature,
    pub inner: Box<[SimpleClassTypeSignature]>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct SimpleClassTypeSignature {
    pub ident: String,
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
    Bounded(TypeBound, ObjectType),
    // <T>
    Normal(ObjectType),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeSignature {
    Base(BaseType),
    Object(ObjectType),
}
