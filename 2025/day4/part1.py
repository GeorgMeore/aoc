from test_input import grid

m, n = len(grid), len(grid[0])
count = 0
for i in range(m):
    for j in range(n):
        if grid[i][j] == '@':
            adj = 0
            for k in (i-1, i, i+1):
                for l in (j-1, j, j+1):
                    if 0 <= k < m and 0 <= l < n and (k, l) != (i, j):
                        adj += grid[k][l] == '@'
            count += adj < 4

print(count)
