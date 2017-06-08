         
#include <stdio.h>
#include <string.h>
#include <set>
#include <queue>
#include <list>
#include <algorithm>
#include <iostream>

using namespace std;

char arr[1001][1001];

class Coordinates {
public:
    int x, y, z;

    void setValues(int, int, int);

    friend std::ostream &operator<<(std::ostream &, const Coordinates &);

    bool operator<(const Coordinates &rhs) const;

    Coordinates(int x, int y, int z);

    Coordinates();
};

Coordinates::Coordinates() {
}

void Coordinates::setValues(int xs, int ys, int zs) {
    x = xs;
    y = ys;
    z = zs;
}

Coordinates::Coordinates(int x, int y, int z) {
    setValues(x, y, z);
}

std::ostream &operator<<(std::ostream &strm, const Coordinates &a) {
    return strm << "Coord(" << a.x << " " << a.y << ")";
}

bool visited[1001][1001][2];
char parent[1001][1001][2];
int cost[1001][1001][2];

void solver(int m, int n, Coordinates start, Coordinates end) {

    int counter = 0;

    int i, j;
    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++) {
            cost[i][j][0] = -1;
            cost[i][j][1] = -1;
            visited[i][j][0] = false;
            visited[i][j][1] = false;
            parent[i][j][0] = 'X';
            parent[i][j][1] = 'X';
        }
    }

    parent[start.x][start.y][0] = 'S';
    visited[start.x][start.y][0] = false;
    cost[start.x][start.y][0] = 0;

    list <Coordinates> list1;
    list <Coordinates> list2;
    list <Coordinates> tmpList1;
    list <Coordinates> tmpList2;

    list1.push_back(start);
    counter = 0;
    while (!list2.empty() || !list1.empty() || !tmpList1.empty() || !tmpList2.empty()) {

        while (!list1.empty()) {
            Coordinates curr = list1.front();
            list1.pop_front();
            if (!visited[curr.x][curr.y][curr.z]) {
                visited[curr.x][curr.y][curr.z] = true;
                cost[curr.x][curr.y][curr.z] = counter;

                if (arr[curr.x][curr.y] == 'W') {
                    tmpList1.push_back(Coordinates(curr.x, curr.y, (curr.z + 1) % 2));
                    parent[curr.x][curr.y][(curr.z + 1) % 2] = 'W';
                }
                if (curr.x - 1 >= 0) {
                    if (!visited[curr.x - 1][curr.y][curr.z] && arr[curr.x - 1][curr.y] != 'X') {
                        if (curr.z == 0)
                            tmpList2.push_back(Coordinates(curr.x - 1, curr.y, curr.z));
                        else
                            tmpList1.push_back(Coordinates(curr.x - 1, curr.y, curr.z));
                        parent[curr.x - 1][curr.y][curr.z] = 'U';
                    }
                }
                if (curr.x + 1 < m) {
                    if (!visited[curr.x + 1][curr.y][curr.z] && arr[curr.x + 1][curr.y] != 'X') {
                        if (curr.z == 0)
                            tmpList2.push_back(Coordinates(curr.x + 1, curr.y, curr.z));
                        else
                            tmpList1.push_back(Coordinates(curr.x + 1, curr.y, curr.z));
                        parent[curr.x + 1][curr.y][curr.z] = 'D';
                    }
                }
                if (curr.y - 1 >= 0) {
                    if (!visited[curr.x][curr.y - 1][curr.z] && arr[curr.x][curr.y - 1] != 'X') {
                        if (curr.z == 0)
                            tmpList2.push_back(Coordinates(curr.x, curr.y - 1, curr.z));
                        else
                            tmpList1.push_back(Coordinates(curr.x, curr.y - 1, curr.z));
                        parent[curr.x][curr.y - 1][curr.z] = 'L';
                    }
                }
                if (curr.y + 1 < n) {
                    if (!visited[curr.x][curr.y + 1][curr.z] && arr[curr.x][curr.y + 1] != 'X') {
                        if (curr.z == 0)
                            tmpList2.push_back(Coordinates(curr.x, curr.y + 1, curr.z));
                        else
                            tmpList1.push_back(Coordinates(curr.x, curr.y + 1, curr.z));
                        parent[curr.x][curr.y + 1][curr.z] = 'R';
                    }
                }
            }
        }
        list1 = tmpList1;
        tmpList1.clear();
        list1.splice(list1.end(), list2);
        list2 = tmpList2;
        tmpList2.clear();

        counter++;
    }
    std::string result("");

    int totalCost = cost[end.x][end.y][0];
	cout << totalCost << endl;
 /*
    Coordinates curr = end;
    while (cost[curr.x][curr.y][curr.z] != 0) {
        int currCost = cost[curr.x][curr.y][curr.z];
        if (curr.z == 0) {
            if (curr.x - 1 >= 0) {
                if (cost[curr.x - 1][curr.y][curr.z] + 2 == currCost) {
                    curr.x = curr.x - 1;
                    result = result + 'D';
                    continue;
                }
            } if (curr.x + 1 < m) {
                if (cost[curr.x + 1][curr.y][curr.z] + 2 == currCost) {
                    curr.x = curr.x + 1;
                    result = result + 'U';
                    continue;
                }
            } if (curr.y - 1 >= 0) {
                if (cost[curr.x][curr.y - 1][curr.z] + 2 == currCost) {
                    curr.y = curr.y - 1;
                    result = result + 'R';
                    continue;
                }
            } if (curr.y + 1 < n) {
                if (cost[curr.x][curr.y + 1][curr.z] + 2 == currCost) {
                    curr.y = curr.y + 1;
                    result = result + 'L';
                    continue;
                }
            } if (cost[curr.x][curr.y][curr.z + 1] + 1 == currCost) {
                curr.z = 1;
                result = result + 'W';
                continue;
            }
        } else if (curr.z == 1) {
            if (curr.x - 1 >= 0) {
                if (cost[curr.x - 1][curr.y][curr.z] + 1 == currCost) {
                    curr.x = curr.x - 1;
                    result = result + 'D';
                    continue;
                }
            } if (curr.x + 1 < m) {
                if (cost[curr.x + 1][curr.y][curr.z] + 1 == currCost) {
                    curr.x = curr.x + 1;
                    result = result + 'U';
                    continue;
                }
            } if (curr.y - 1 >= 0) {
                if (cost[curr.x][curr.y - 1][curr.z] + 1 == currCost) {
                    curr.y = curr.y - 1;
                    result = result + 'R';
                    continue;
                }
            } if (curr.y + 1 < n) {
                if (cost[curr.x][curr.y + 1][curr.z] + 1 == currCost) {
                    curr.y = curr.y + 1;
                    result = result + 'L';
                    continue;
                }
            } if (cost[curr.x][curr.y][0] + 1 == currCost) {
                curr.z = 0;
                result = result + 'W';
                continue;
            }
        }
    }

    // reverse string
    //std::reverse(result.begin(), result.end());
    std::cout << totalCost << " " << result << std::endl;
	*/
}


int main(int argc, const char *argv[]) {

    int  i = 0, j, m = 0, flag = 1;
    /************ Read file ***************/
    const char *fileName = argv[1];
    FILE *file = fopen(fileName, "r");

    while (fgets(arr[i], sizeof(arr[i]), file)) {
        if (flag) {
            m = strlen(arr[i]);
            if (arr[i][m - 1] == '\n')
                m--;
        }
        i++;
    }
    int n = i;
	m = strlen(arr[0])-1;
    fclose(file);
	cout << m << n << endl;

    /************ Read file ***************/

    Coordinates start, end;
    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++) {
            if (arr[i][j] == 'S')
                start.setValues(i, j, 0);
            if (arr[i][j] == 'E')
                end.setValues(i, j, 0);
        }
    }
    solver(n, m, start, end);
}
        
