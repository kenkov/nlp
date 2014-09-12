#include <iostream>
#include <array>
using namespace std;


const int DIM = 2;

typedef struct Point {
    double point[DIM];
    int value;
} Point;


double innerProduct(const int dim, const double *x, const double *y) {
    int sum = 0;
    for (int i = 0; i < dim; i++) {
        sum += x[i] * y[i];
    }
    return sum;
}

double l2norm(const int dim, const double *x) {
    return innerProduct(dim, x, x);
}

string show(const int dim, const double *w) {
    string ans = "w = ";
    for (int i = 0; i < dim; i++) {
        ans += (to_string(w[i]) + string(", "));
    }
    ans += " b = ";
    ans += to_string(w[dim]);
    return ans;
}

void perceptron(const int dim, const Point *points, const int size, double *ans) {
    double b = 0;
    // find max R^2
    double R = -1;
    for (int i = 0; i < size; i++) {
        double x = l2norm(dim, points[i].point);
        if (x > R) {
            R = x;
        }
    }

    // perceptron
    for (unsigned int k = 0; k < 10e4; k++) {
        for (int i = 0; i < size; i++) {
            int value = points[i].value;
            const double *point = points[i].point;
            if (value * innerProduct(dim, point, ans) + b <= 0) {
                for (int j = 0; j < dim; j++) {
                    ans[j] = ans[j] + point[j] * value;
                }
                b = b + value * R;
            }
        }
    }
    ans[dim] = b;
    return;
}

int main(void) {
    Point points[] = {
        {{0, 0}, -1},
        {{1, 0}, -1},
        {{2, 0}, -1},
        {{0, 1}, 1},
        {{1, 1}, 1},
        {{2, 1}, 1},
    };
    unsigned int pointSize = (sizeof(points) / sizeof(Point));
    double ans[DIM+1];

    perceptron(DIM, points, pointSize, ans);
    cout << show(DIM, ans) << endl;

    return 0;
}
