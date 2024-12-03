import numpy as np
import numpy.typing as npt


# Read report from line
def parse_report(line: str):
    return np.array(list(map(int, line.strip().split())), dtype=np.int64)


# Check if report is safe
def is_safe(report: npt.NDArray[np.int64]):
    diffs = np.diff(report)
    signs = np.sign(diffs)
    abs_diffs = np.abs(diffs)
    return (
        np.all(signs[1:] == signs[0])
        and np.all(abs_diffs >= 1)
        and np.all(abs_diffs <= 3)
    )


# Generate dampened report candidates
def dampened_candidates(report: npt.NDArray[np.int64]):
    return (np.delete(report, k) for k in range(len(report)))


# Check if any of the dampened report candidates are safe
def is_safe_dampened(report: npt.NDArray[np.int64]):
    # Perform test on dampened candidates
    return np.any(list(map(is_safe, dampened_candidates(report))))


# Part 1: Calculate the number of safe reports
def part1(reports: list[npt.NDArray[np.int64]]):
    return sum(map(is_safe, reports))


# Part 2: Calculate the number of safe reports using dampening if necessary
def part2(reports: list[npt.NDArray[np.int64]]):
    return sum(map(lambda report: is_safe(report) or is_safe_dampened(report), reports))


def main():
    # Load input
    with open("../inputs/day-02.txt", "r") as file:
        reports = list(map(parse_report, file.readlines()))
    # print("Input data:\n", data)

    print("Part 1:", part1(reports))
    print("Part 2:", part2(reports))


if __name__ == "__main__":
    main()
