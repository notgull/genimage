// Copyright 2023 John Nunley
//
// This file is part of genimage.
// 
// genimage is free software: you can redistribute it and/or modify it 
// under the terms of the GNU Affero General Public License as published by 
// the Free Software Foundation, either version 3 of the License, or (at your 
// option) any later version.
// 
// genimage is distributed in the hope that it will be useful, but 
// WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
// or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
// for more details.
// 
// You should have received a copy of the GNU Affero General Public License 
// along with genimage. If not, see <https://www.gnu.org/licenses/>. 

//! Easy-to-use iterators over arrays.

/// Iterators over arrays that allow us to avoid an edition/MSRV bump.
macro_rules! set_count_iterator {
    () => {};
    (
        @enum_variants
        [],
        $len:expr,
    ) => {};
    (
        @enum_variants
        [$name: ident $(,)? $($incoming: ident),*],
        $len:expr,
    ) => {
        type $name<T> = [T; $len];

        set_count_iterator! {
            @enum_variants
            [$($incoming),*],
            ($len) + 1,
        }
    };
    (
        $(#[$meta:meta])*
        $vis: vis $name: ident : [$($field_name: ident),* $(,)*] ($modname: ident);
        $($tt: tt)*
    ) => {
        mod $modname {
            use core::mem;

            set_count_iterator! {
                @enum_variants
                [$($field_name),*],
                1,
            }

            $(#[$meta])*
            #[doc(hidden)]
            #[derive(Debug, Copy, Clone, Hash)]
            $vis enum $name <T> {
                $(
                    $field_name($field_name <T>),
                )*
                Empty
            }

            impl<T> $name<T> {
                /// Create an empty iterator.
                pub fn empty() -> Self {
                    $name::Empty
                }
            }

            $(
                impl<T> From<$field_name<T>> for $name<T> {
                    fn from(value: $field_name<T>) -> Self {
                        $name::$field_name(value)
                    }
                }
            )*

            impl<T> From<[T; 0]> for $name<T> {
                fn from(_: [T; 0]) -> Self {
                    $name::Empty
                }
            }

            impl<T> Iterator for $name<T> {
                type Item = T;

                fn next(&mut self) -> Option<Self::Item> {
                    match mem::replace(self, Self::Empty) {
                        Self::Empty => None,
                        $(
                            Self::$field_name([result, rest @ ..]) => {
                                *self = Self::from(rest);
                                Some(result)
                            }
                        )*
                    }
                }

                fn size_hint(&self) -> (usize, Option<usize>) {
                    match self {
                        Self::Empty => (0, Some(0)),
                        $(
                            Self::$field_name(t) => (t.len(), Some(t.len())),
                        )*
                    }
                }

                fn count(self) -> usize {
                    match self {
                        Self::Empty => 0,
                        $(
                            Self::$field_name(t) => t.len(),
                        )*
                    }
                }

                fn last(self) -> Option<Self::Item> {
                    match self {
                        Self::Empty => None,
                        $(
                            Self::$field_name([.., last]) => Some(last),
                        )*
                    }
                }

                fn fold<B, F>(self, init: B, mut f: F) -> B
                where
                    F: FnMut(B, Self::Item) -> B,
                {
                    match self {
                        Self::Empty => init,
                        $(
                            Self::$field_name(t) => {
                                let mut accum = init;
                                for item in t {
                                    accum = f(accum, item);
                                }
                                accum
                            }
                        )*
                    }
                }
            }

            impl<T> core::iter::FusedIterator for $name<T> {}

            impl<T> ExactSizeIterator for $name<T> {
                fn len(&self) -> usize {
                    match self {
                        Self::Empty => 0,
                        $(
                            Self::$field_name(t) => t.len(),
                        )*
                    }
                }
            }

            impl<T> DoubleEndedIterator for $name<T> {
                fn next_back(&mut self) -> Option<Self::Item> {
                    match mem::replace(self, Self::Empty) {
                        Self::Empty => None,
                        $(
                            Self::$field_name([rest @ .., result]) => {
                                *self = Self::from(rest);
                                Some(result)
                            }
                        )*
                    }
                }
            }
        }

        $vis use $modname::$name;

        set_count_iterator! {
            $($tt)*
        }
    }
}

set_count_iterator! {
    pub(crate) Two: [A, B] (two_impl);
    pub(crate) Three: [A, B, C] (three_impl);
    pub(crate) Four: [A, B, C, D] (four_impl);
    pub(crate) Five: [A, B, C, D, E] (five_impl);
}

