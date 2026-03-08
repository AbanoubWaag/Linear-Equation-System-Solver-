#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <cmath>
#include <algorithm>

using namespace std;

// Helper to convert float to string without trailing zeros
string to_str(float f) {
    ostringstream oss;
    oss << f;
    return oss.str();
}

// Structure to hold a variable and its coefficient
struct Term {
    string var;
    float coeff;
};

// --- Equation Class ---
class Equation {
public:
    vector<Term> terms;
    float constant;

    Equation() : constant(0.0f) {}

    // Parses a raw string equation into the Proper Form
    void parse(const string& raw) {
        terms.clear();
        constant = 0.0f;
        int side = 1; // 1 for LHS, -1 for RHS
        int i = 0;
        int n = raw.length();

        while (i < n) {
            if (raw[i] == ' ') { i++; continue; }
            if (raw[i] == '=') { side = -1; i++; continue; }

            float sign = 1.0f;
            if (raw[i] == '+') { sign = 1.0f; i++; }
            else if (raw[i] == '-') { sign = -1.0f; i++; }

            while (i < n && raw[i] == ' ') i++;

            string num_str = "";
            while (i < n && (isdigit(raw[i]) || raw[i] == '.')) {
                num_str += raw[i];
                i++;
            }
            float val = 1.0f;
            if (!num_str.empty()) val = stof(num_str);

            string var_name = "";
            while (i < n && isalnum(raw[i])) {
                var_name += raw[i];
                i++;
            }

            if (var_name.empty()) {
                // It's a constant term. Move to RHS.
                constant += (side == 1 ? -1 : 1) * sign * val;
            }
            else {
                // It's a variable. Move to LHS.
                add_term(var_name, (side == 1 ? 1 : -1) * sign * val);
            }
        }
        normalize();
    }

    void add_term(const string& name, float val) {
        for (int i = 0; i < terms.size(); i++) {
            if (terms[i].var == name) {
                terms[i].coeff += val;
                return;
            }
        }
        terms.push_back({ name, val });
    }

    void normalize() {
        vector<Term> filtered;
        for (int i = 0; i < terms.size(); i++) {
            if (abs(terms[i].coeff) > 1e-6) {
                filtered.push_back(terms[i]);
            }
        }
        terms = filtered;

        // Sort alphabetically
        for (int i = 0; i < terms.size(); i++) {
            for (int j = i + 1; j < terms.size(); j++) {
                if (terms[j].var < terms[i].var) {
                    Term temp = terms[i];
                    terms[i] = terms[j];
                    terms[j] = temp;
                }
            }
        }
    }

    float get_coeff(const string& var_name) const {
        for (int i = 0; i < terms.size(); i++) {
            if (terms[i].var == var_name) return terms[i].coeff;
        }
        return 0.0f;
    }

    void print() const {
        string res = "";
        if (terms.empty()) {
            res += "0";
        }
        else {
            for (int i = 0; i < terms.size(); i++) {
                if (terms[i].coeff > 0 && i > 0) res += "+";
                res += to_str(terms[i].coeff) + terms[i].var;
            }
        }
        res += "=" + to_str(constant);
        cout << res << endl;
    }
};

// --- EquationSystem Class ---
class EquationSystem {
public:
    vector<Equation> eqns;

    void add_equation(const string& str) {
        Equation eq;
        eq.parse(str);
        eqns.push_back(eq);
    }

    vector<string> get_all_vars() const {
        vector<string> all_vars;
        for (int i = 0; i < eqns.size(); i++) {
            for (int j = 0; j < eqns[i].terms.size(); j++) {
                bool found = false;
                for (int k = 0; k < all_vars.size(); k++) {
                    if (all_vars[k] == eqns[i].terms[j].var) {
                        found = true; break;
                    }
                }
                if (!found) all_vars.push_back(eqns[i].terms[j].var);
            }
        }
        // Sort alphabetically
        for (int i = 0; i < all_vars.size(); i++) {
            for (int j = i + 1; j < all_vars.size(); j++) {
                if (all_vars[j] < all_vars[i]) {
                    string temp = all_vars[i];
                    all_vars[i] = all_vars[j];
                    all_vars[j] = temp;
                }
            }
        }
        return all_vars;
    }

    void print_num_vars() const {
        cout << get_all_vars().size() << endl;
    }

    void print_column(const string& var) const {
        for (int i = 0; i < eqns.size(); i++) {
            cout << to_str(eqns[i].get_coeff(var)) << endl;
        }
    }

    Equation add_equations(int i, int j) const {
        Equation res;
        for (int k = 0; k < eqns[i - 1].terms.size(); k++)
            res.add_term(eqns[i - 1].terms[k].var, eqns[i - 1].terms[k].coeff);
        for (int k = 0; k < eqns[j - 1].terms.size(); k++)
            res.add_term(eqns[j - 1].terms[k].var, eqns[j - 1].terms[k].coeff);
        res.constant = eqns[i - 1].constant + eqns[j - 1].constant;
        res.normalize();
        return res;
    }

    Equation subtract_equations(int i, int j) const {
        Equation res;
        for (int k = 0; k < eqns[i - 1].terms.size(); k++)
            res.add_term(eqns[i - 1].terms[k].var, eqns[i - 1].terms[k].coeff);
        for (int k = 0; k < eqns[j - 1].terms.size(); k++)
            res.add_term(eqns[j - 1].terms[k].var, -eqns[j - 1].terms[k].coeff);
        res.constant = eqns[i - 1].constant - eqns[j - 1].constant;
        res.normalize();
        return res;
    }

    Equation substitute(const string& var, int i, int j) const {
        float coeff_j = eqns[j - 1].get_coeff(var);
        if (abs(coeff_j) < 1e-6) return eqns[i - 1]; // Cannot substitute if missing
        float coeff_i = eqns[i - 1].get_coeff(var);
        float multiplier = coeff_i / coeff_j;

        Equation res;
        for (int k = 0; k < eqns[i - 1].terms.size(); k++)
            res.add_term(eqns[i - 1].terms[k].var, eqns[i - 1].terms[k].coeff);
        for (int k = 0; k < eqns[j - 1].terms.size(); k++)
            res.add_term(eqns[j - 1].terms[k].var, -multiplier * eqns[j - 1].terms[k].coeff);
        res.constant = eqns[i - 1].constant - multiplier * eqns[j - 1].constant;
        res.normalize();
        return res;
    }

    vector<vector<float> > get_D_matrix() const {
        vector<string> vars = get_all_vars();
        int n = eqns.size();
        int m = vars.size();
        vector<vector<float> > mat(n, vector<float>(m, 0));
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                mat[i][j] = eqns[i].get_coeff(vars[j]);
            }
        }
        return mat;
    }

    void print_matrix(const vector<vector<float> >& mat) const {
        for (int i = 0; i < mat.size(); i++) {
            for (int j = 0; j < mat[i].size(); j++) {
                cout << to_str(mat[i][j]) << (j == mat[i].size() - 1 ? "" : " ");
            }
            cout << endl;
        }
    }

    vector<vector<float> > get_Dx_matrix(const string& var) const {
        vector<vector<float> > mat = get_D_matrix();
        vector<string> vars = get_all_vars();
        int col_idx = -1;
        for (int j = 0; j < vars.size(); j++) {
            if (vars[j] == var) { col_idx = j; break; }
        }
        if (col_idx != -1) {
            for (int i = 0; i < mat.size(); i++) {
                mat[i][col_idx] = eqns[i].constant;
            }
        }
        return mat;
    }

    float calc_determinant(vector<vector<float> > mat) const {
        int n = mat.size();
        if (n == 0 || mat[0].size() != n) return 0.0f; // Must be square

        float det = 1.0f;
        for (int i = 0; i < n; i++) {
            int pivot = i;
            for (int j = i + 1; j < n; j++) {
                if (abs(mat[j][i]) > abs(mat[pivot][i])) pivot = j;
            }
            if (abs(mat[pivot][i]) < 1e-6) return 0.0f;
            if (pivot != i) {
                swap(mat[i], mat[pivot]);
                det *= -1;
            }
            det *= mat[i][i];
            for (int j = i + 1; j < n; j++) {
                float factor = mat[j][i] / mat[i][i];
                for (int k = i; k < n; k++) {
                    mat[j][k] -= factor * mat[i][k];
                }
            }
        }
        return det;
    }

    void solve() const {
        float det_D = calc_determinant(get_D_matrix());
        if (abs(det_D) < 1e-6) {
            cout << "No Solution" << endl;
            return;
        }
        vector<string> vars = get_all_vars();
        for (int i = 0; i < vars.size(); i++) {
            float det_Dx = calc_determinant(get_Dx_matrix(vars[i]));
            cout << vars[i] << "=" << to_str(det_Dx / det_D) << endl;
        }
    }
};

int main() {
    int n;
    if (!(cin >> n)) return 0;
    string dummy;
    getline(cin, dummy); // Consume newline

    EquationSystem sys;
    for (int i = 0; i < n; i++) {
        string eq_str;
        getline(cin, eq_str);
        sys.add_equation(eq_str);
    }

    string cmd_line;
    while (getline(cin, cmd_line)) {
        if (cmd_line.empty()) continue;
        stringstream ss(cmd_line);
        string cmd;
        ss >> cmd;

        if (cmd == "quit") {
            break;
        }
        else if (cmd == "num_vars") {
            sys.print_num_vars();
        }
        else if (cmd == "equation") {
            int i; ss >> i;
            sys.eqns[i - 1].print();
        }
        else if (cmd == "column") {
            string x; ss >> x;
            sys.print_column(x);
        }
        else if (cmd == "add") {
            int i, j; ss >> i >> j;
            sys.add_equations(i, j).print();
        }
        else if (cmd == "subtract") {
            int i, j; ss >> i >> j;
            sys.subtract_equations(i, j).print();
        }
        else if (cmd == "substitute") {
            string x; int i, j;
            ss >> x >> i >> j;
            sys.substitute(x, i, j).print();
        }
        else if (cmd == "D") {
            string arg;
            if (ss >> arg) sys.print_matrix(sys.get_Dx_matrix(arg));
            else sys.print_matrix(sys.get_D_matrix());
        }
        else if (cmd == "D_value") {
            cout << to_str(sys.calc_determinant(sys.get_D_matrix())) << endl;
        }
        else if (cmd == "solve") {
            sys.solve();
        }
    }
    return 0;
}