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

//! A crate for manipulating images and image formats.

#![no_std]

#[cfg(test)]
extern crate std;

pub mod format;
pub mod pixel;

mod iter;

/// The endianness of a pixel or image.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Endianness {
    /// Little endian.
    Little,

    /// Big endian.
    Big,
}

impl Endianness {
    /// Get the native endianness of the system.
    pub const fn native() -> Self {
        #[cfg(target_endian = "little")]
        {
            Self::Little
        }

        #[cfg(target_endian = "big")]
        {
            Self::Big
        }
    }
}
