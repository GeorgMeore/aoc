from test_input import grid

m, n = len(grid), len(grid[0])

def neighbors(i, j):
    return [
        (k, l)
        for k in (i-1, i, i+1)
        for l in (j-1, j, j+1)
        if 0 <= k < m and 0 <= l < n and (k != i or l != j)
    ]

counts = [[0 for _ in range(n)] for _ in range(m)]
movable = []
for i in range(n):
    for j in range(m):
        if grid[i][j] == '@':
            count = 0
            for k, l in neighbors(i, j):
                count += grid[k][l] == '@'
            counts[i][j] = count
            if count < 4:
                movable.append((i, j))
removed = 0
while movable:
    i, j = movable.pop()
    removed += 1
    for k, l in neighbors(i, j):
        if counts[k][l] == 4:
            movable.append((k, l))
        counts[k][l] -= 1

print(removed)
