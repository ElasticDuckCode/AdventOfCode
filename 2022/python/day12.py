from pprint import pprint
from time import sleep


def calculate_distance_vals(h1, h2):
    if h2 == ord("E"):
        d = ord("z") - h1
    elif h1 == ord("S"):
        d = h2 - ord("a")
    else:
        d = h2 - h1
    return float("inf") if (d >= 2) else max(d, 1)


def calculate_grid_distance(data, p1, p2):
    h1 = ord(data[p1[0]][p1[1]])
    h2 = ord(data[p2[0]][p2[1]])
    return calculate_distance_vals(h1, h2)


def find(data, val):
    for i, line in enumerate(data):
        j = line.find(val)
        if j > -1:
            return (i, j)
    return -1


def initial_distance(data):
    m = len(data)
    n = len(data[0])
    idx = [(i, j) for j in range(n) for i in range(m)]
    val = [float("inf") for j in range(n) for i in range(m)]
    dists = dict(zip(idx, val))
    start = find(data, "S")
    dists[start] = 0
    return dists


def update_distance(grid, prev, dists, neighbor, curr, dist):
    if neighbor in dists:
        old = dists[neighbor]
        new = dist + calculate_grid_distance(grid, curr, neighbor)
        if new < old:
            dists[neighbor] = new
            prev[neighbor] = curr


def get_arrow(prev, curr):
    i, j = curr
    l, m = prev
    if (l - i) > 0:
        ch = "^"
    elif (l - i) < 0:
        ch = "v"
    elif (m - j) > 0:
        ch = "<"
    elif (m - j) < 0:
        ch = ">"
    else:
        ch = "*"
    return ch


def dijkstras(grid, end, dists, prev):
    while True:
        dbg = list(reversed(sorted(dists.items(), key=lambda item: item[1])))
        pprint(dbg[-5:])
        m, n = len(grid), len(grid[0])
        i, j = min(dists, key=dists.get)
        dist = dists.pop((i, j))

        if i + 1 < m:
            update_distance(grid, prev, dists, (i + 1, j), (i, j), dist)
        if i - 1 >= 0:
            update_distance(grid, prev, dists, (i - 1, j), (i, j), dist)
        if j + 1 < n:
            update_distance(grid, prev, dists, (i, j + 1), (i, j), dist)
        if j - 1 >= 0:
            update_distance(grid, prev, dists, (i, j - 1), (i, j), dist)

        pgrid = grid
        pgrid = [list("." * len(s)) for s in pgrid]
        # pgrid = [list(s) for s in pgrid]
        pgrid[i][j] = "X"
        point = prev[(i, j)]
        x, y = i, j
        while point is not None:
            ch = get_arrow(point, (x, y))
            x, y = point
            pgrid[x][y] = ch
            point = prev[(x, y)]
        pgrid = ["".join(p) for p in pgrid]
        pgrid = "\n".join(pgrid)
        print(pgrid)
        print()
        sleep(0.00)

        if i == end[0] and j == end[1]:
            path = [(i, j)]
            point = prev[(i, j)]
            while point is not None:
                path.append(point)
                point = prev[point]
            path.reverse()
            return path

    # return dijkstras(grid, end, dists, prev)


def main():
    with open("inputs/input12.txt", "r") as f:
        data = f.read()
        data = data.splitlines()
    dist = dijkstras(
        data, find(data, "E"), initial_distance(data), {find(data, "S"): None}
    )
    # pprint(dist)
    print(len(dist))


if __name__ == "__main__":
    main()
