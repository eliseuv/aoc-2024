import re
from typing import Generator

DAY_03_PART_1_TEST = (
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
)

DAY_03_PART_2_TEST = (
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
)

MUL_COMMAND: re.Pattern[str] = re.compile(r"mul\((\d{1,3}),(\d{1,3})\)")


def eval_mul(mul_match: re.Match[str]):
    return int(mul_match[1]) * int(mul_match[2])


# Search all `mul` commands, evaluate them and sum the results
def part1(program: str):
    return sum(map(eval_mul, re.finditer(MUL_COMMAND, program)))


# Returns the smallest possible range with for a given start value and list of possible end values
def smallest_range(start: int, end_candidates: list[int], end_max: int):
    try:
        return (start, next(end for end in end_candidates if end > start))
    except StopIteration:
        return (start, end_max)


# Evaluate `mul` commands skipping over ignored sections and sum the results
def part2(program: str):
    dont_idx = [m.start() for m in re.finditer(r"don't\(\)", program)]
    do_idx = [m.end() for m in re.finditer(r"do\(\)", program)]
    ignored_sections = [
        smallest_range(start, do_idx, len(program)) for start in dont_idx
    ]

    return sum(
        map(
            eval_mul,
            filter(
                lambda m: not any(
                    m.start() >= a and m.end() <= b for (a, b) in ignored_sections
                ),
                re.finditer(MUL_COMMAND, program),
            ),
        )
    )


def main():
    # Load input
    with open("../inputs/day-03.txt", "r") as file:
        program = file.read()
    # program = DAY_03_PART_2_TEST
    # print("Input data:\n", program)

    print("Part 1:", part1(program))
    print("Part 2:", part2(program))


if __name__ == "__main__":
    main()
