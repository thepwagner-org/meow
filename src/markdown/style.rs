//! Terminal styling for markdown output.
//!
//! Dracula-inspired color scheme for pretty terminal rendering.

use termimad::crossterm::style::{Attribute, Color};
use termimad::MadSkin;

/// Create a skin for rendering markdown output.
///
/// If `use_color` is false, returns a plain unstyled skin.
pub fn skin(use_color: bool) -> MadSkin {
    if !use_color {
        return MadSkin::no_style();
    }

    let mut skin = MadSkin::default();

    // H1 - purple #b379ed, left-aligned, no underline
    skin.headers[0].set_fg(Color::Rgb {
        r: 0xb3,
        g: 0x79,
        b: 0xed,
    });
    skin.headers[0].align = termimad::Alignment::Left;
    skin.headers[0]
        .compound_style
        .remove_attr(Attribute::Underlined);

    // H2 - lighter purple #d38ef5, no underline
    skin.headers[1].set_fg(Color::Rgb {
        r: 0xd3,
        g: 0x8e,
        b: 0xf5,
    });
    skin.headers[1]
        .compound_style
        .remove_attr(Attribute::Underlined);

    // Bold - orange #ffb86c
    skin.bold.set_fg(Color::Rgb {
        r: 0xff,
        g: 0xb8,
        b: 0x6c,
    });

    // Italic - yellow #f1fa8c
    skin.italic.set_fg(Color::Rgb {
        r: 0xf1,
        g: 0xfa,
        b: 0x8c,
    });

    // Inline code - teal #38a3a5
    skin.inline_code.set_fg(Color::Rgb {
        r: 0x38,
        g: 0xa3,
        b: 0xa5,
    });

    skin
}
