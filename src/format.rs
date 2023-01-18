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

use core::{cmp, fmt, hash, iter::FusedIterator};
use crate::iter::Four;

/// The format for an image.
///
/// Images can be encoded in many different formats. This structure provides
/// information about the format of an image. Currently, this information
/// includes:
///
/// - The number of bits per pixel for an image. This is the number of bits
///   that are used to encode a single pixel. The valid values for bits per
///   pixel are 1, 4, 8, 16, 24, 32, 96 and 128.
/// - The ordering of colors within the image. For instance, an image may
///   be of the `ARGB` format, which means the order of colors in a pixel
///   is first alpha, then red, then green, finally blue.
/// - The number of bits per each color channel. This indicates the number
///   of bits that are used to encode each color channel.
///
/// Note that `genimage` only supports channel encodings consisting of red,
/// blue, green and alpha. This may be changed in the future.
///
/// ## Usage
///
/// New formats can be created using `Format::new()`.
///
/// ```
/// use genimage::format::{ColorType, Format};
///
/// // create a 32-bit RGBA format with 8 bits per channel
/// let rgba32 = Format::new(
///     32, // bits per pixel
///     ColorType::Rgba, // rgba
///     8, // # of alpha bits
///     8, // # of red bits
///     8, // # of green bits
///     8, // # of blue bits
/// );
/// # let _ = rgba32;
/// ```
///
/// However, it is generally recommended to use one of the standard
/// formats that `genimage` comes with. These formats are associated
/// constants on the `Format` struct:
///
/// ```
/// use genimage::format::Format;
///
/// // equivalent to the above
/// let rgba32 = Format::RGBA32;
/// # let _ = rgba32;
/// ```
///
/// There are also formats for images made up of floating point numbers:
///
/// ```
/// # use genimage::format::Format;
/// let my_format = Format::ARGB_F32;
/// # let _ = my_format;
/// ```
#[derive(Debug, Copy, Clone)]
pub struct Format {
    /// The bits per pixel for this image.
    bpp: u8,

    /// The color type for this image.
    color_type: ColorType,

    /// The bits for each channel of the image.
    channels: Channels,
}

pub(crate) const MAX_BITS_PER_PIXEL: usize = 32 * 4;
pub(crate) const MAX_BYTES_PER_PIXEL: usize = MAX_BITS_PER_PIXEL / 8;

impl PartialEq for Format {
    fn eq(&self, other: &Self) -> bool {
        self.bpp == other.bpp
            && self.channels().eq(other.channels())
    }

    #[allow(clippy::partialeq_ne_impl)]
    fn ne(&self, other: &Self) -> bool {
        self.bpp != other.bpp
            || self.channels().ne(other.channels())
    }
}

impl Eq for Format {}

impl PartialOrd for Format {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Format {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.bpp.cmp(&other.bpp)
            .then_with(|| self.channels().cmp(other.channels()))
    }
}

impl hash::Hash for Format {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.bpp.hash(state);
        self.channels().for_each(|chan| chan.hash(state));
    }
}

impl Format {
    /// The ARGB32 format.
    pub const ARGB32: Format = Format::new(32, ColorType::Argb, 8, 8, 8, 8);
    /// The XRGB32 format.
    pub const XRGB32: Format = Format::new(32, ColorType::Argb, 0, 8, 8, 8);
    /// The ABGR32 format.
    pub const ABGR32: Format = Format::new(32, ColorType::Abgr, 8, 8, 8, 8);
    /// The XBGR32 format.
    pub const XBGR32: Format = Format::new(32, ColorType::Abgr, 0, 8, 8, 8);
    /// The RGBA32 format.
    pub const RGBA32: Format = Format::new(32, ColorType::Rgba, 8, 8, 8, 8);
    /// The BGRA32 format.
    pub const BGRA32: Format = Format::new(32, ColorType::Bgra, 8, 8, 8, 8);

    /// The RGB24 format.
    pub const RGB24: Format = Format::new(24, ColorType::Argb, 0, 8, 8, 8);
    /// The BGR24 format.
    pub const BGR24: Format = Format::new(24, ColorType::Abgr, 0, 8, 8, 8);

    /// The ARGB16 format.
    pub const ARGB16: Format = Format::new(16, ColorType::Argb, 4, 4, 4, 4);
    /// The XRGB16 format.
    pub const XRGB16: Format = Format::new(16, ColorType::Argb, 0, 4, 4, 4);
    /// The ABGR16 format.
    pub const ABGR16: Format = Format::new(16, ColorType::Abgr, 4, 4, 4, 4);
    /// The XBGR16 format.
    pub const XBGR16: Format = Format::new(16, ColorType::Abgr, 0, 4, 4, 4);

    /// The A8 format.
    pub const A8: Format = Format::new(8, ColorType::Alpha, 8, 0, 0, 0);

    /// The A4 format.
    pub const A4: Format = Format::new(4, ColorType::Alpha, 4, 0, 0, 0);

    /// The A1 format.
    pub const A1: Format = Format::new(1, ColorType::Alpha, 1, 0, 0, 0);

    /// ARGB with 32-bit floats.
    pub const ARGB_F32: Format = Format::new(32 * 4, ColorType::ArgbFloat, 32, 32, 32, 32);
    /// RGB with 32-bit floats.
    pub const RGB_F32: Format = Format::new(32 * 3, ColorType::ArgbFloat, 0, 32, 32, 32);

    /// Create a new format with the given specifications.
    ///
    /// ## Parameters
    ///
    /// - `bpp` is the bits per pixel for the format. The valid values for this
    ///   parameter are 1, 4, 8, 16, 24, 32, 96 and 128. If the value is not
    ///   valid, it will be rounded up or down (preferably up) to another entry.
    /// - `color_type` is the color type for the format.
    /// - `*_bits` is used to encode the number of bits used for each channel.
    ///   Valid values are 0 through 8, 10, 16 and 32. If the value is not
    ///   one of these, logic errors will occur, up to and including panics.
    pub const fn new(
        bpp: u8,
        color_type: ColorType,
        alpha_bits: u8,
        red_bits: u8,
        green_bits: u8,
        blue_bits: u8,
    ) -> Self {
        // round the bpp appropriately
        let bpp = match bpp {
            0..=1 => 1,
            2..=4 => 4,
            5..=8 => 8,
            9..=16 => 16,
            17..=24 => 24,
            25..=32 => 32,
            33..=96 => 96,
            _ => 128,
        };

        Self {
            bpp,
            color_type,
            channels: Channels::new(alpha_bits, red_bits, green_bits, blue_bits),
        }
    }

    /// The bits per pixel for this image.
    pub const fn bpp(&self) -> u8 {
        self.bpp
    }

    /// Number of bytes per pixel.
    ///
    /// This is the number of bytes required to encode a pixel.
    pub const fn bytes(&self) -> u8 {
        match self.bpp {
            1 | 4 => 1,
            bpp => bpp / 8,
        }
    }

    /// If the size of a pixel is less than a byte.
    pub const fn subbyte(&self) -> bool {
        self.bpp < 8
    }

    /// The color type for this image.
    pub const fn color_type(&self) -> ColorType {
        self.color_type
    }

    /// The number of bits used in the alpha channel.
    pub const fn alpha_bits(&self) -> u8 {
        self.channels.alpha()
    }

    /// Does this image use floating point numbers?
    pub fn involves_float(&self) -> bool {
        self.color_type.involves_float()
    }

    /// The number of bits used in the red channel.
    pub const fn red_bits(&self) -> u8 {
        self.channels.red()
    }

    /// The number of bits used in the green channel.
    pub const fn green_bits(&self) -> u8 {
        self.channels.green()
    }

    /// The number of bits used in the blue channel.
    pub const fn blue_bits(&self) -> u8 {
        self.channels.blue()
    }

    const fn bits_for_channel(&self, channel: Channel) -> u8 {
        match channel {
            Channel::Alpha => self.alpha_bits(),
            Channel::Red => self.red_bits(),
            Channel::Green => self.green_bits(),
            Channel::Blue => self.blue_bits(),
        }
    }

    const fn total_bits(&self) -> u8 {
        self.alpha_bits() + self.red_bits() + self.green_bits() + self.blue_bits()
    }

    /// Iterate over the channels for this format.
    ///
    /// Each `ChannelInfo` structure encodes the channel involved,
    /// as well as the number of bits used for that channel and the
    /// shift that would be needed to reach that channel, if the pixel
    /// were encoded in a primitive value.
    ///
    /// The channels are returned in order.
    pub fn channels(
        self,
    ) -> impl FusedIterator<Item = ChannelInfo> + ExactSizeIterator + DoubleEndedIterator {
        ChannelIter::new(self, self.color_type().channels())
    }
}

/// Iterator over the channels of this format.
///
/// I could've used an standard library iterator, but this let me make it `ExactSizeIterator` and 
/// `DoubleEndedIterator`, which a standard `filter_map()` wouldn't let me do.
#[derive(Debug)]
struct ChannelIter<I: ?Sized> {
    format: Format,

    /// Current shift from the front.
    shift: u8,

    /// Current shift from the back.
    shift_back: u8,

    /// The inner iterator over channels.
    channels: I,
}

impl<I> ChannelIter<I> {
    fn new(format: Format, channels: I) -> Self {
        Self {
            format,
            shift: format.total_bits(),
            shift_back: 0,
            channels,
        }
    }
}

impl<I: ExactSizeIterator<Item = Channel>> Iterator for ChannelIter<I> {
    type Item = ChannelInfo;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // If we've met the other end of the iterator, return.
            if self.shift == self.shift_back {
                return None;
            }

            // Get the next channel.
            let channel = self.channels.next()?;

            // Get the number of bits for that channel.
            let bits = self.format.bits_for_channel(channel);

            // if the number of bits is zero, skip it
            if bits == 0 {
                continue;
            }

            // Create the channel info.
            let info = ChannelInfo {
                bits,
                shift: self.shift - bits,
                channel,
            };

            // Decrement the shift.
            self.shift -= bits;

            // Return the channel info.
            return Some(info);
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let l = self.len();
        (l, Some(l))
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        self.len()
    }
}

impl<I: FusedIterator<Item = Channel> + ExactSizeIterator> FusedIterator for ChannelIter<I> {}
impl<I: ExactSizeIterator<Item = Channel>> ExactSizeIterator for ChannelIter<I> {
    fn len(&self) -> usize {
        // Determine which channels we need to work with from this point on.
        let mut current_shift = self.format.bpp;

        self.format
            .color_type()
            .channels()
            .filter(|channel| {
                let bits = self.format.bits_for_channel(*channel);
                current_shift -= bits;
                current_shift < self.shift && current_shift > self.shift_back
            })
            .count()
    }
}

impl<I: DoubleEndedIterator<Item = Channel> + ExactSizeIterator> DoubleEndedIterator
    for ChannelIter<I>
{
    fn next_back(&mut self) -> Option<Self::Item> {
        loop {
            // if we've met the other end of the iterator, return
            if self.shift == self.shift_back {
                return None;
            }

            // get the next channel
            let channel = self.channels.next_back()?;

            // get the number of bits for that channel
            let bits = self.format.bits_for_channel(channel);

            // if the number of bits is zero, skip it
            if bits == 0 {
                continue;
            }

            // create the channel info
            let info = ChannelInfo {
                bits,
                shift: self.shift_back,
                channel,
            };

            // decrement the shift
            self.shift_back += bits;

            // return the channel info
            return Some(info);
        }
    }
}

/// The color type for this image.
///
/// This defines the channels that appear for the format, as well
/// as the order that they appear in.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
#[non_exhaustive]
pub enum ColorType {
    /// Packed ARGB tuple.
    Argb,

    /// Packed RGBA tuple.
    Rgba,

    /// Packed ABGR tuple.
    Abgr,

    /// Packed BGRA tuple.
    Bgra,

    /// Single alpha channel.
    Alpha,

    /// Tuple of 32-bit floats.
    ///
    /// This implies that the bit count for each component is either 32
    /// or 0. If either of these are not the case, this may lead to panics
    /// or rounding, but never unsafe behavior.
    ArgbFloat,
}

impl ColorType {
    /// The channels used for this color type.
    ///
    /// This will iterate over the channels in the order that they
    /// appear in the color type.
    pub fn channels(
        self,
    ) -> impl ExactSizeIterator<Item = Channel> + DoubleEndedIterator + FusedIterator {
        use Channel::*;

        match self {
            ColorType::Argb => Four::from([Alpha, Red, Green, Blue]),
            ColorType::Rgba => Four::from([Red, Green, Blue, Alpha]),
            ColorType::Abgr => Four::from([Alpha, Blue, Green, Red]),
            ColorType::Bgra => Four::from([Blue, Green, Red, Alpha]),
            ColorType::Alpha => Four::from([Alpha]),
            ColorType::ArgbFloat => Four::from([Alpha, Red, Green, Blue]),
        }
    }

    /// Whether or not this color type involves floats.
    pub fn involves_float(self) -> bool {
        core::matches!(self, ColorType::ArgbFloat)
    }
}

/// Information about a format's channel.
///
/// This is provided by the [`channels()`] method, and provides
/// information on how to access the channels of a pixel.
///
/// [`channels()`]: crate::Format::channels
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ChannelInfo {
    /// The number of bits used in this channel.
    pub bits: u8,

    /// The number of bits to shift to get this channel.
    pub shift: u8,

    /// The type of the channel.
    pub channel: Channel,
}

/// The type of channel.
///
/// This is provided by the [`channels()`] method, and indicates
/// the channel that is being dealt with.
///
/// [`channels()`]: crate::Format::channels
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum Channel {
    /// Red channel.
    Red,

    /// Green channel.
    Green,

    /// Blue channel.
    Blue,

    /// Alpha channel.
    Alpha,
}

impl Default for Channel {
    fn default() -> Self {
        Channel::Alpha
    }
}

/// The number of bits each channel has.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Channels(u16);

/// Convert the real number of bits to its representation in `Channels`.
#[inline]
const fn convert_to_channels_repr(num_bits: u8) -> u16 {
    match num_bits {
        0..=8 => num_bits as u16,
        10 => 9,
        16 => 10,
        32 => 11,
        _ => {
            panic!("Invalid number of bits")
        }
    }
}

/// Convert the representation of `Channels` to the real number of bits.
#[inline]
const fn convert_from_channels_repr(num_bits: u16) -> u8 {
    match num_bits {
        0..=8 => num_bits as u8,
        9 => 10,
        10 => 16,
        11 => 32,
        _ => {
            panic!("Invalid number of bits")
        }
    }
}

const ALPHA_SHIFT: u16 = 12;
const RED_SHIFT: u16 = 8;
const GREEN_SHIFT: u16 = 4;
const BLUE_SHIFT: u16 = 0;
const CHANNEL_MASK: u16 = 0x0f;

impl Channels {
    const fn new(alpha: u8, red: u8, green: u8, blue: u8) -> Channels {
        Channels(
            (convert_to_channels_repr(alpha) << ALPHA_SHIFT)
                | (convert_to_channels_repr(red) << RED_SHIFT)
                | (convert_to_channels_repr(green) << GREEN_SHIFT)
                | (convert_to_channels_repr(blue) << BLUE_SHIFT),
        )
    }

    const fn alpha(&self) -> u8 {
        convert_from_channels_repr((self.0 >> ALPHA_SHIFT) & CHANNEL_MASK)
    }

    const fn red(&self) -> u8 {
        convert_from_channels_repr((self.0 >> RED_SHIFT) & CHANNEL_MASK)
    }

    const fn green(&self) -> u8 {
        convert_from_channels_repr((self.0 >> GREEN_SHIFT) & CHANNEL_MASK)
    }

    const fn blue(&self) -> u8 {
        convert_from_channels_repr((self.0 >> BLUE_SHIFT) & CHANNEL_MASK)
    }
}

impl fmt::Debug for Channels {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Channels")
            .field("alpha", &self.alpha())
            .field("red", &self.red())
            .field("green", &self.green())
            .field("blue", &self.blue())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::{Format, ChannelInfo, Channel, ColorType};
    use core::mem::size_of;
    use std::vec::Vec;

    #[test]
    fn format_properties() {
        // Format should be smaller than 32 bits and should be able to be niched.
        assert!(size_of::<Format>() <= size_of::<i32>());
        assert_eq!(size_of::<Format>(), size_of::<Option<Format>>());
    }

    #[test]
    fn smoke_channel_info() {
        macro_rules! test_channel_info {
            ($format:expr, $chans:expr) => {{
                let chans = $format.channels().collect::<Vec<_>>();
                assert_eq!(chans.as_slice(), &$chans);
            }}
        }

        test_channel_info!(Format::ARGB32, [
            ChannelInfo {
                bits: 8,
                channel: Channel::Alpha,
                shift: 24,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Red,
                shift: 16,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Green,
                shift: 8,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Blue,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::XRGB32, [
            ChannelInfo {
                bits: 8,
                channel: Channel::Red,
                shift: 16,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Green,
                shift: 8,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Blue,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::ABGR32, [
            ChannelInfo {
                bits: 8,
                channel: Channel::Alpha,
                shift: 24,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Blue,
                shift: 16,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Green,
                shift: 8,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Red,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::XBGR32, [
            ChannelInfo {
                bits: 8,
                channel: Channel::Blue,
                shift: 16,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Green,
                shift: 8,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Red,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::RGBA32, [
            ChannelInfo {
                bits: 8,
                channel: Channel::Red,
                shift: 24,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Green,
                shift: 16,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Blue,
                shift: 8,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Alpha,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::BGRA32, [
            ChannelInfo {
                bits: 8,
                channel: Channel::Blue,
                shift: 24,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Green,
                shift: 16,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Red,
                shift: 8,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Alpha,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::RGB24, [
            ChannelInfo {
                bits: 8,
                channel: Channel::Red,
                shift: 16,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Green,
                shift: 8,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Blue,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::BGR24, [
            ChannelInfo {
                bits: 8,
                channel: Channel::Blue,
                shift: 16,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Green,
                shift: 8,
            },
            ChannelInfo {
                bits: 8,
                channel: Channel::Red,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::ARGB16, [
            ChannelInfo {
                bits: 4,
                channel: Channel::Alpha,
                shift: 12,
            },
            ChannelInfo {
                bits: 4,
                channel: Channel::Red,
                shift: 8,
            },
            ChannelInfo {
                bits: 4,
                channel: Channel::Green,
                shift: 4,
            },
            ChannelInfo {
                bits: 4,
                channel: Channel::Blue,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::XRGB16, [
            ChannelInfo {
                bits: 4,
                channel: Channel::Red,
                shift: 8,
            },
            ChannelInfo {
                bits: 4,
                channel: Channel::Green,
                shift: 4,
            },
            ChannelInfo {
                bits: 4,
                channel: Channel::Blue,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::ABGR16, [
            ChannelInfo {
                bits: 4,
                channel: Channel::Alpha,
                shift: 12,
            },
            ChannelInfo {
                bits: 4,
                channel: Channel::Blue,
                shift: 8,
            },
            ChannelInfo {
                bits: 4,
                channel: Channel::Green,
                shift: 4,
            },
            ChannelInfo {
                bits: 4,
                channel: Channel::Red,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::XBGR16, [
            ChannelInfo {
                bits: 4,
                channel: Channel::Blue,
                shift: 8,
            },
            ChannelInfo {
                bits: 4,
                channel: Channel::Green,
                shift: 4,
            },
            ChannelInfo {
                bits: 4,
                channel: Channel::Red,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::A8, [
            ChannelInfo {
                bits: 8,
                channel: Channel::Alpha,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::A4, [
            ChannelInfo {
                bits: 4,
                channel: Channel::Alpha,
                shift: 0,
            },
        ]);

        test_channel_info!(Format::A1, [
            ChannelInfo {
                bits: 1,
                channel: Channel::Alpha,
                shift: 0,
            },
        ]);
    }
}
