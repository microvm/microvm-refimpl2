import re
from typing import List, Union, Tuple, Any, Callable, TypeVar
from typing.re import Pattern

Predicate = Union[str,
        Tuple[Pattern, ...],
        Callable[[Any], bool]]

def _string_contains(line, string):
    return string in line

def _pattern_value_match(line, tup):
    pat = tup[0]
    vals = tup[1:]
    m = pat.search(line)
    return m is not None and all(
            v is None or g == v
            for g,v in zip(m.groups(), vals))

def _apply_func(line, func):
    return func(line)

def find_line(lines: List[str], substr: Predicate, start: int = 0) -> int:
    """Find the line that contains or matches substr since line ``start``. """
    if isinstance(substr, str):
        pred = _string_contains
    elif isinstance(substr, tuple):
        pred = _pattern_value_match
    else:
        pred = _apply_func

    for i in range(start, len(lines)):
        if pred(lines[i], substr):
            return i

    raise KeyError("Not found: " + str(substr) + "\n text:" + str(lines) )

def extract_lines(parent: str, begin: Predicate, end: Predicate) -> str:
    """
    Extract the lines between the line containing ``begin`` and the line
    containing ``end`` (excluding both lines) in ``parent``.
    """
    lines = parent.splitlines()

    begin_line = find_line(lines, begin)
    end_line = find_line(lines, end, begin_line+1)

    new_lines = lines[begin_line+1:end_line]

    return "\n".join(new_lines)

def inject_lines(parent: str, begin: Predicate, end: Predicate, generated: str) -> str:
    """
    Replace the lines between the line containing ``begin`` and the line
    containing ``end`` (excluding both lines) in ``parent`` with ``generated``.
    """
    lines = parent.splitlines()

    begin_line = find_line(lines, begin)
    end_line = find_line(lines, end, begin_line+1)

    new_lines = lines[:begin_line+1] + generated.splitlines() + lines[end_line:]

    return "\n".join(new_lines)
