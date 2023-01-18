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

//! Utilities for deriving pixels from raw bytes.

use core::iter::FusedIterator;
use core::slice::Iter as SliceIter;
use crate::format::{Format, Channel};

/// A pixel that is a maximum of 32 bits wide.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pixel32 {
    /// The format of this pixel.
    format: Format,

    /// The raw bytes of this pixel.
    data: u32,
}

impl Pixel32 {
    /// Create a new pixel from the given format and raw bytes.
    pub const fn new(format: Format, data: u32) -> Self {
        Self { format, data }
    }

    /// Get the format of this pixel.
    pub const fn format(self) -> Format {
        self.format
    }

    /// Get the raw bytes of this pixel.
    pub const fn data(self) -> u32 {
        self.data
    }

    /// Get the value of the given channel.
    /// 
    /// Returns `None` if the channel is not present in the format.
    pub fn channel(self, channel: Channel) -> Option<u8> {
        self.format.channels().find(|c| c.channel == channel)
            .map(|info| {
                let shift = info.shift;
                let mask = (1 << info.bits) - 1;

                ((self.data >> shift) & mask) as u8
            })
    }

    /// Get the red channel of this pixel.
    pub fn red(self) -> Option<u8> {
        self.channel(Channel::Red)
    }

    /// Get the green channel of this pixel.
    pub fn green(self) -> Option<u8> {
        self.channel(Channel::Green)
    }

    /// Get the blue channel of this pixel.
    pub fn blue(self) -> Option<u8> {
        self.channel(Channel::Blue)
    }

    /// Get the alpha channel of this pixel.
    pub fn alpha(self) -> Option<u8> {
        self.channel(Channel::Alpha)
    }

    /// Set the value of a given channel.
    /// 
    /// Returns `false` if this pixel does not have the given channel.
    pub fn set_channel(&mut self, channel: Channel, value: u8) -> bool {
        self.format.channels().find(|c| c.channel == channel)
            .map(|info| {
                let shift = info.shift;
                let mask = (1 << info.bits) - 1;

                self.data &= !(mask << shift);
                self.data |= ((value as u32) & mask) << shift;
            })
            .is_some()
    }

    /// Set the red channel of this pixel.
    pub fn set_red(&mut self, value: u8) -> bool {
        self.set_channel(Channel::Red, value)
    }

    /// Set the green channel of this pixel.
    pub fn set_green(&mut self, value: u8) -> bool {
        self.set_channel(Channel::Green, value)
    }

    /// Set the blue channel of this pixel.
    pub fn set_blue(&mut self, value: u8) -> bool {
        self.set_channel(Channel::Blue, value)
    }

    /// Set the alpha channel of this pixel.
    pub fn set_alpha(&mut self, value: u8) -> bool {
        self.set_channel(Channel::Alpha, value)
    }

    /// Set this pixel to the value of another pixel of a different format.
    /// 
    /// Returns the number of channels that were successfully set.
    pub fn set(&mut self, other: Pixel32) -> usize {
        // If the formats are the same, the data should be too.
        if self.format == other.format {
            self.data = other.data;
            return self.format.channels().count();
        }

        // Merge the formats over.
        self.format.channels().filter_map(|c| {
            other.channel(c.channel).map(|v| {
                let shift = c.shift;
                let mask = (1 << c.bits) - 1;

                self.data &= !(mask << shift);
                self.data |= ((v as u32) & mask) << shift;
            })
        }).count()
    }
}
