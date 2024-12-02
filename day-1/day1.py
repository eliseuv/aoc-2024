import re
import numpy as np
import numpy.typing as npt

# Regex pattern of two integers separated by some whitespace
pattern = re.compile(r"^(\d+)\W+(\d+)\n")


# Extract number pair from line
def get_pair(line: str) -> tuple[np.int64, np.int64] | None:
    match = re.match(pattern, line)
    if not match:
        return None
    left, right = match.groups()
    return (np.int64(left), np.int64(right))


# Count the number of occurrences of each number of a given array
def get_counts(arr: npt.NDArray[np.int64]) -> dict[np.int64, np.int64]:
    counts: dict[np.int64, np.int64] = {}
    for x in arr:
        counts[x] = counts.get(x, np.int64(0)) + 1
    return counts


def main():
    # Read input file to matrix
    with open("./input", "r") as file:
        matrix = np.array([get_pair(line) for line in file.readlines()], dtype=np.int64)
    print("Input data:\n", matrix)

    # Separate the two lists
    left = matrix[:, 0]
    right = matrix[:, 1]

    # Calculate total distance
    total_distance: np.int64 = np.sum(np.abs(np.sort(left) - np.sort(right)))
    print("Total distance:", total_distance)

    # Calculate the similarity score
    rcounts = get_counts(right)
    similarity_score = np.sum(
        np.fromiter(
            (
                lcount * lvalue * rcounts.get(lvalue, np.int64(0))
                for lvalue, lcount in get_counts(left).items()
            ),
            np.int64,
        )
    )
    print("Similarity score:", similarity_score)


if __name__ == "__main__":
    main()
