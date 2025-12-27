#!/usr/bin/env python3
"""
Utility to convert every R script in the scripts/ folder into PDFs with a header
identifying the source file.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple

from pygments import lex
from pygments.lexers import get_lexer_by_name
from pygments.style import Style
from pygments.styles import get_style_by_name
from pygments.token import Token
from reportlab.lib import colors
from reportlab.lib.pagesizes import letter
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfgen import canvas


@dataclass
class Layout:
    pagesize: tuple[float, float] = letter
    margin: float = 50
    header_offset: float = 28
    body_font: str = "Courier"
    body_size: int = 10
    header_font: str = "Helvetica-Bold"
    header_size: int = 16
    body_leading: float = 12
    code_padding: float = 8
    code_bg: colors.Color = colors.HexColor("#f5f5f5")
    code_border: colors.Color = colors.HexColor("#cccccc")
    tab_size: int = 4
    style_name: str = "friendly"

    def __post_init__(self) -> None:
        self.body_leading = self.body_size + 2


Segment = Tuple[str, colors.Color]
Line = List[Segment]


def style_color(style: Style, token_type: Token) -> colors.Color:
    """Resolve the RGB color for a given token."""
    attributes = style.style_for_token(token_type)
    hex_color = attributes.get("color")
    if not hex_color:
        return colors.black
    return colors.HexColor(f"#{hex_color}")


def tokenize_lines(
    content: str, lexer, style: Style, tab_size: int
) -> list[list[Segment]]:
    """Convert script content into lines of colorized segments."""
    lines: list[list[Segment]] = [[]]
    for token_type, value in lex(content, lexer):
        if not value:
            continue
        color = style_color(style, token_type)
        parts = value.split("\n")
        for index, part in enumerate(parts):
            normalized = part.replace("\t", " " * tab_size)
            if normalized:
                lines[-1].append((normalized, color))
            if index < len(parts) - 1:
                lines.append([])
    if not lines:
        lines.append([])
    return lines


def wrap_segments(segments: Sequence[Segment], max_chars: int) -> Iterable[Line]:
    """Wrap a single line of colored segments to the max column width."""
    if not segments:
        yield []
        return

    current: Line = []
    used = 0

    for text, color in segments:
        remaining_text = text
        while remaining_text:
            space = max_chars - used
            if space <= 0:
                yield current
                current = []
                used = 0
                space = max_chars
            chunk = remaining_text[:space]
            current.append((chunk, color))
            used += len(chunk)
            remaining_text = remaining_text[space:]
    if current:
        yield current


def wrap_lines(lines: Sequence[Line], max_chars: int) -> list[Line]:
    wrapped: list[Line] = []
    for segments in lines:
        if not segments:
            wrapped.append([])
            continue
        wrapped.extend(list(wrap_segments(segments, max_chars)))
    if not wrapped:
        wrapped.append([])
    return wrapped


def render_script_to_canvas(
    script_path: Path,
    pdf_canvas: canvas.Canvas,
    layout: Layout,
    initial_page: bool = False,
    lexer=None,
    style: Style | None = None,
) -> None:
    """Render script contents onto an existing canvas."""
    if lexer is None or style is None:
        raise ValueError("Lexer and style must be provided for syntax highlighting.")
    width, height = layout.pagesize
    header_text = script_path.name
    char_width = pdfmetrics.stringWidth("M", layout.body_font, layout.body_size)
    max_chars = max(1, int((width - 2 * layout.margin) // char_width))
    script_text = script_path.read_text(encoding="utf-8")
    colored_lines = wrap_lines(
        tokenize_lines(script_text, lexer, style, layout.tab_size), max_chars
    )

    def start_page(initial: bool = False) -> float:
        if not initial:
            pdf_canvas.showPage()
        pdf_canvas.setFont(layout.header_font, layout.header_size)
        top = height - layout.margin
        pdf_canvas.drawString(layout.margin, top, header_text)

        block_left = layout.margin - layout.code_padding
        block_bottom = layout.margin - layout.code_padding
        block_width = width - 2 * layout.margin + 2 * layout.code_padding
        block_top = top - layout.header_offset + layout.code_padding
        block_height = block_top - block_bottom

        pdf_canvas.setFillColor(layout.code_bg)
        pdf_canvas.setStrokeColor(layout.code_border)
        pdf_canvas.roundRect(
            block_left, block_bottom, block_width, block_height, radius=6, fill=1, stroke=0
        )
        pdf_canvas.setStrokeColor(layout.code_border)
        pdf_canvas.roundRect(
            block_left, block_bottom, block_width, block_height, radius=6, fill=0, stroke=1
        )

        pdf_canvas.setFillColor(colors.black)
        pdf_canvas.setFont(layout.body_font, layout.body_size)
        return block_top - layout.code_padding

    text_y = start_page(initial=initial_page)
    for segments in colored_lines:
        if text_y < layout.margin:
            text_y = start_page()
        cursor_x = layout.margin
        for text, color in segments:
            pdf_canvas.setFillColor(color)
            pdf_canvas.drawString(cursor_x, text_y, text)
            cursor_x += char_width * len(text)
        text_y -= layout.body_leading


def render_individual_scripts(
    scripts: list[Path],
    output_dir: Path,
    layout: Layout,
    lexer,
    style: Style,
) -> list[Path]:
    """Render each script into its own PDF."""
    output_dir.mkdir(parents=True, exist_ok=True)
    pdf_paths: list[Path] = []
    for script in scripts:
        pdf_path = output_dir / f"{script.stem}.pdf"
        c = canvas.Canvas(str(pdf_path), pagesize=layout.pagesize)
        c.setTitle(script.name)
        render_script_to_canvas(
            script, c, layout, initial_page=True, lexer=lexer, style=style
        )
        c.save()
        pdf_paths.append(pdf_path)
    return pdf_paths


def render_combined_pdf(
    scripts: list[Path],
    output_path: Path,
    layout: Layout,
    lexer,
    style: Style,
) -> Path:
    """Render all scripts into a single PDF."""
    output_path.parent.mkdir(parents=True, exist_ok=True)
    c = canvas.Canvas(str(output_path), pagesize=layout.pagesize)
    if scripts:
        c.setTitle(f"{len(scripts)} R scripts")
    first = True
    for script in scripts:
        render_script_to_canvas(
            script, c, layout, initial_page=first, lexer=lexer, style=style
        )
        first = False
    c.save()
    return output_path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Convert R scripts in a folder to PDFs."
    )
    parser.add_argument(
        "--source",
        type=Path,
        default=Path("scripts"),
        help="Folder containing .R files (default: scripts)",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("script_pdfs"),
        help="Folder for individual PDFs (default: script_pdfs)",
    )
    parser.add_argument(
        "--combined-output",
        type=Path,
        default=Path("scripts.pdf"),
        help="Path to the merged PDF (default: scripts.pdf)",
    )
    parser.add_argument(
        "--mode",
        choices=("combined", "individual", "both"),
        default="combined",
        help="Generate a merged PDF, separate PDFs, or both (default: combined)",
    )
    parser.add_argument(
        "--style",
        default=None,
        help="Pygments style name for syntax highlighting (default: layout setting)",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if not args.source.exists():
        raise SystemExit(f"Source folder {args.source} does not exist.")
    scripts = sorted(args.source.glob("*.R"))
    if not scripts:
        print(f"No .R files found in {args.source}")
        return

    layout = Layout()
    if args.style:
        layout.style_name = args.style

    lexer = get_lexer_by_name("r", stripnl=False, ensurenl=True)
    style = get_style_by_name(layout.style_name)
    created: list[Path] = []

    if args.mode in {"individual", "both"}:
        created.extend(
            render_individual_scripts(scripts, args.output_dir, layout, lexer, style)
        )

    if args.mode in {"combined", "both"}:
        created.append(
            render_combined_pdf(scripts, args.combined_output, layout, lexer, style)
        )

    print("Created PDF files:")
    for path in created:
        print(f" - {path}")


if __name__ == "__main__":
    main()
