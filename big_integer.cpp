//
// Created by rsbat on 3/17/18.
//

#include <string>
#include <limits>

#include "big_integer.h"

typedef unsigned int ui;

static const unsigned UI_MAX = std::numeric_limits<unsigned >::max();

template<typename T>
ui uicast(const T &x) {
    return static_cast<ui>(x);
}

unsigned long long ullcast(unsigned int x) {
    return static_cast<unsigned long long>(x);
}

big_integer::big_integer() : isNegative(false), number(0) {
}

big_integer::big_integer(int a) : isNegative(a < 0), number(1) {
    number[0] = uicast(a);
    removeLeadingZeros(); // if a == 0
}

big_integer::big_integer(ui a) : isNegative(false), number(1) {
    number[0] = a;
    removeLeadingZeros();
}

big_integer::big_integer(std::string const &str) :big_integer() {
    big_integer ans;
    for (char ch : str) {
        if (ch == '-') { continue; }
        ans = ans * 10 + (ch - '0');
    }
    if (str[0] == '-') {
        ans = -ans;
    }
    swap(*this, ans);
}

big_integer::~big_integer() = default;

big_integer &big_integer::operator+=(big_integer const &rhs) {
    unsigned long long carry = 0;
    size_t n = std::max(number.size(), rhs.number.size()) + 1;
    std::vector<ui> res(n);

    for (size_t i = 0; i < n; i++) {
        unsigned long long tmp = carry + at(i) + rhs.at(i);
        res[i] = uicast(tmp);
        carry = tmp >> 32;
    }

    number.swap(res);
    isNegative = static_cast<bool>(number.back() >> 31);
    removeLeadingZeros();

    return *this;
}

big_integer &big_integer::operator-=(big_integer const &rhs) {
    unsigned long long carry = 1;
    size_t n = std::max(number.size(), rhs.number.size()) + 1;
    std::vector<ui> res(n);

    for (size_t i = 0; i < n; i++) {
        unsigned long long tmp = carry + at(i) + ~rhs.at(i);
        res[i] = uicast(tmp);
        carry = tmp >> 32;
    }

    number.swap(res);
    isNegative = static_cast<bool>(number.back() >> 31);
    removeLeadingZeros();

    return *this;
}

big_integer &big_integer::operator*=(big_integer const &rhs) {
    big_integer a_rhs = abs(rhs);
    big_integer a_lhs = abs(*this);
    size_t n = a_lhs.number.size();
    size_t m = a_rhs.number.size();

    std::vector<ui> result(n + m + 1, 0);

    for (size_t i = 0; i < m; i++) {
        unsigned long long carry = 0;
        for (size_t j = 0; j < n || carry; j++) {
            unsigned long long tmp = carry + result[i + j] + ullcast(a_lhs.at(j)) * a_rhs.number[i];
            result[i + j] = uicast(tmp);
            carry = tmp >> 32;
        }
    }

    a_lhs.number.swap(result);
    a_lhs.isNegative = this->isNegative ^ rhs.isNegative;
    if (a_lhs.isNegative) {
        a_lhs.isNegative = false;
        a_lhs = -a_lhs;
    }
    a_lhs.removeLeadingZeros();

    swap(*this, a_lhs);
    return *this;
}

void big_integer::sub_with_shift(big_integer& lhs, const big_integer& rhs, long sh) {
    unsigned long long carry = 1;
    size_t m = rhs.number.size() + sh;
    size_t n = std::max(lhs.number.size(), rhs.number.size() + sh) + 1;
    std::vector<ui> res(n);

    for (size_t i = 0; i < (size_t)sh; i++) {
        unsigned long long tmp = carry + lhs.number[i] + UI_MAX;
        res[i] = uicast(tmp);
        carry = tmp >> 32;
    }

    for (size_t i = sh; i < m; i++) {
        unsigned long long tmp = carry + lhs.number[i] + ~rhs.number[i - sh];
        res[i] = uicast(tmp);
        carry = tmp >> 32;
    }

    for (size_t i = m; i < n; i++) {
        unsigned long long tmp = carry + lhs.at(i) + UI_MAX;
        res[i] = uicast(tmp);
        carry = tmp >> 32;
    }

    lhs.number.swap(res);
    lhs.isNegative = static_cast<bool>(lhs.number.back() >> 31);
    lhs.removeLeadingZeros();
}

big_integer &big_integer::operator/=(big_integer const &rhs) {
    if (number.size() < rhs.number.size()) {
        number.clear();
        isNegative = false;
        return *this;
    }

    if (number.empty() && !isNegative) {
        return *this;
    }

    big_integer a_rhs = abs(rhs);
    big_integer a_lhs = abs(*this);

    int sh = 0;
    unsigned int tmp_bk = a_rhs.number.back();
    while (tmp_bk < (1u << 31)) {
        tmp_bk <<= 1;
        sh++;
    }
    a_rhs <<= sh;
    a_lhs <<= sh;

    size_t m = a_lhs.number.size() - a_rhs.number.size();
    size_t n = a_rhs.number.size();
    std::vector<ui> res(m + 1, 0);

    big_integer tmp1 = radix_shl(a_rhs, m);
    if (a_lhs >= tmp1) {
        a_lhs -= tmp1;
        res[m] = 1;
    }

    for (long i = m - 1; i >= 0; i--) {
        unsigned long long q = ((ullcast(a_lhs.at(n + i)) << 32) + a_lhs.at(n + i - 1)) / a_rhs.number.back();
        q = std::min(q, ullcast(UI_MAX));

        if (!a_lhs.isNegative && a_lhs.number.empty()) {
            q = 0;
        } else {
            sub_with_shift(a_lhs, uicast(q) * a_rhs, i);
            while (a_lhs.isNegative) {
                q--;
                a_lhs += radix_shl(a_rhs, i);
            }
        }
        res[i] = uicast(q);
    }

    a_lhs.number.swap(res);
    a_lhs.isNegative = isNegative ^ rhs.isNegative;
    if (a_lhs.isNegative) {
        a_lhs.isNegative = false;
        a_lhs = -a_lhs;
    }
    a_lhs.removeLeadingZeros();

    swap(*this, a_lhs);
    return *this;
}

big_integer &big_integer::operator%=(big_integer const &rhs) {
    big_integer ans = *this - (*this / rhs) * rhs;
    swap(*this, ans);
    return *this;
}

big_integer &big_integer::operator&=(big_integer const &rhs) {
    big_integer cpy(*this);

    size_t n = std::max(cpy.number.size(), rhs.number.size());
    for (size_t i = 0; i < n; i++) {
        if (i == cpy.number.size()) { cpy.number.push_back(cpy.isNegative ? UI_MAX : 0); }
        cpy.number[i] &= rhs.at(i);
    }
    cpy.isNegative &= rhs.isNegative;
    cpy.removeLeadingZeros();

    swap(*this, cpy);
    return *this;
}

big_integer &big_integer::operator|=(big_integer const &rhs) {
    big_integer cpy(*this);

    size_t n = std::max(cpy.number.size(), rhs.number.size());
    for (size_t i = 0; i < n; i++) {
        if (i == cpy.number.size()) { cpy.number.push_back(cpy.isNegative ? UI_MAX : 0); }
        cpy.number[i] |= rhs.at(i);
    }
    cpy.isNegative |= rhs.isNegative;
    cpy.removeLeadingZeros();

    swap(*this, cpy);
    return *this;
}

big_integer &big_integer::operator^=(big_integer const &rhs) {
    big_integer cpy(*this);

    size_t n = std::max(cpy.number.size(), rhs.number.size());
    for (size_t i = 0; i < n; i++) {
        if (i == cpy.number.size()) { cpy.number.push_back(cpy.isNegative ? UI_MAX : 0); }
        cpy.number[i] ^= rhs.at(i);
    }
    cpy.isNegative ^= rhs.isNegative;
    cpy.removeLeadingZeros();

    swap(*this, cpy);
    return *this;
}

big_integer &big_integer::operator<<=(int rhs) {
    big_integer res(*this);

    if (res.number.empty() && res.isNegative) {
        res.number.push_back(UI_MAX);
    }

    res = radix_shl(res, rhs / 32);
    rhs %= 32;

    unsigned int carry = 0;
    for (unsigned int &i : res.number) {
        unsigned long long tmp = (ullcast(i) << rhs) + carry;
        i = uicast(tmp);
        carry = uicast(tmp >> 32);
    }

    if (carry) {
        unsigned long long tmp = (ullcast(res.isNegative ? UI_MAX : 0) << rhs) + carry;
        res.number.push_back(uicast(tmp));
    }


    res.removeLeadingZeros();
    swap(*this, res);
    return *this;
}

big_integer &big_integer::operator>>=(int rhs) {
    if (number.empty()) { return *this; }

    big_integer res(*this);

    if (res.number.empty() && res.isNegative) {
        res.number.push_back(UI_MAX);
    }

    res = radix_shr(res, rhs / 32);
    rhs %= 32;

    unsigned int carry = 0;
    ui carryMask = (1u << rhs) - 1;

    auto fst = static_cast<int>(res.number.back());
    carry = uicast(fst & carryMask);
    fst >>= rhs;
    res.number[(int) res.number.size() - 1] = uicast(fst);

    for (int i = (int) res.number.size() - 2; i >= 0; i--) {
        unsigned int tcarry = res.number[i] & carryMask;
        res.number[i] >>= rhs;
        res.number[i] += carry << (32 - rhs);
        carry = tcarry;
    }

    res.removeLeadingZeros();
    swap(*this, res);
    return *this;
}

big_integer big_integer::operator+() const {
    return *this;
}

big_integer big_integer::operator-() const {
    if (!isNegative && number.empty()) {
        return *this;
    }

    big_integer res(*this);
    res.number.push_back(res.isNegative ? UI_MAX : 0);
    unsigned long long carry = 1;
    for (unsigned int &i : res.number) {
        unsigned long long tmp = carry + ~i;
        i = uicast(tmp);
        carry = tmp >> 32;
    }

    res.isNegative ^= 1;
    res.removeLeadingZeros();
    return res;
}

big_integer big_integer::operator~() const {
    big_integer res(*this);
    for (auto &x : res.number) {
        x = ~x;
    }
    res.isNegative = !res.isNegative;
    return res;
}

big_integer &big_integer::operator++() {
    return *this += 1;
}

big_integer big_integer::operator++(int) {
    big_integer tmp(*this);
    *this += 1;
    return tmp;
}

big_integer &big_integer::operator--() {
    return *this -= 1;
}

big_integer big_integer::operator--(int) {
    big_integer tmp(*this);
    *this -= 1;
    return tmp;
}

bool operator==(big_integer const &a, big_integer const &b) {
    if (a.isNegative != b.isNegative) { return false; }
    if (a.number.size() != b.number.size()) { return false; }

    for (size_t i = 0; i < a.number.size(); i++) {
        if (a.number[i] != b.number[i]) { return false; }
    }

    return true;
}

bool operator!=(big_integer const &a, big_integer const &b) {
    return !(a == b);
}

bool operator<(big_integer const &a, big_integer const &b) {
    if (a.isNegative != b.isNegative) { return a.isNegative; }
    if (a.number.size() < b.number.size()) { return !a.isNegative; }
    if (a.number.size() > b.number.size()) { return a.isNegative; }

    int n = (int) a.number.size() - 1;
    for (int i = n; i >= 0; i--) {
        if (a.number[i] < b.number[i]) {
            return !a.isNegative;
        }
        if (a.number[i] > b.number[i]) {
            return a.isNegative;
        }
    }

    return false;
}

bool operator>(big_integer const &a, big_integer const &b) {
    return b < a;
}

bool operator<=(big_integer const &a, big_integer const &b) {
    return !(a > b);
}

bool operator>=(big_integer const &a, big_integer const &b) {
    return b <= a;
}

std::string to_string(big_integer const &a) {
    std::string s;
    big_integer tmp(a);

    if (a.isNegative) {
        s += '-';
        tmp = -tmp;
    }

    if (tmp.number.empty()) {
        return "0";
    }

    std::vector<char> ans;

    while (!tmp.number.empty()) {
        big_integer symb = tmp % 10;

        if (symb.number.empty()) {
            ans.push_back('0');
        } else {
            ans.push_back(char('0' + symb.number[0]));
        }

        tmp /= 10;
    }

    while (!ans.empty() && ans.back() == '0') { ans.pop_back(); }
    while (!ans.empty()) {
        s += ans.back();
        ans.pop_back();
    }

    return s;
}

void big_integer::removeLeadingZeros() noexcept {
    while (!number.empty() && ((!isNegative && number.back() == 0) || (isNegative && number.back() == UI_MAX))) {
        number.pop_back();
    }
}


big_integer operator+(big_integer a, big_integer const &b) {
    return a += b;
}

big_integer operator-(big_integer a, big_integer const &b) {
    return a -= b;
}

big_integer operator*(big_integer a, big_integer const &b) {
    return a *= b;
}

big_integer operator/(big_integer a, big_integer const &b) {
    return a /= b;
}

big_integer operator%(big_integer a, big_integer const &b) {
    return a %= b;
}

big_integer operator&(big_integer a, big_integer const &b) {
    return a &= b;
}

big_integer operator|(big_integer a, big_integer const &b) {
    return a |= b;
}

big_integer operator^(big_integer a, big_integer const &b) {
    return a ^= b;
}

big_integer operator<<(big_integer a, int b) {
    return a <<= b;
}

big_integer operator>>(big_integer a, int b) {
    return a >>= b;
}

std::ostream &operator<<(std::ostream &s, big_integer const &a) {
    s << to_string(a);
    return s;
}

big_integer abs(const big_integer& x) {
    if (x.isNegative) {
        return -x;
    }
    return x;
}

big_integer operator*(big_integer a, int b) {
    return a *= b;
}

big_integer operator*(int a, big_integer b) {
    return b *= a;
}

big_integer operator*(big_integer a, unsigned b) {
    return a *= b;
}

big_integer operator*(unsigned a, big_integer b) {
    return b *= a;
}

unsigned int big_integer::at(size_t pos) const noexcept {
    if (pos < number.size()) {
        return number[pos];
    }
    return isNegative ? UI_MAX : 0;
}

big_integer &big_integer::operator*=(int rhs) {
    big_integer res(*this);
    if (rhs < 0) {
        res = -res;
    }
    res *= uicast(std::abs(rhs));

    swap(*this, res);
    return *this;
}

big_integer &big_integer::operator*=(unsigned rhs) {
    big_integer a_lhs = abs(*this);

    std::vector<ui> result(a_lhs.number.size() + 2, 0);

    unsigned long long carry = 0;
    size_t n = a_lhs.number.size();
    for (size_t j = 0; j < n || carry; j++) {
        unsigned long long tmp = carry + result[j] + ullcast(a_lhs.at(j)) * rhs;
        result[j] = uicast(tmp);
        carry = tmp >> 32;
    }

    a_lhs.number.swap(result);
    a_lhs.isNegative = isNegative;
    if (a_lhs.isNegative) {
        a_lhs.isNegative = false;
        a_lhs = -a_lhs;
    }
    a_lhs.removeLeadingZeros();

    swap(*this, a_lhs);
    return *this;
}

void swap(big_integer &lhs, big_integer &rhs) {
    lhs.number.swap(rhs.number);
    std::swap(lhs.isNegative, rhs.isNegative);
}

big_integer big_integer::radix_shl(const big_integer& x, long sh) {
    big_integer res;
    size_t n = x.number.size();

    res.number.resize(n + sh);
    for (size_t i = 0; i < n; i++) {
        res.number[i + sh] = x.number[i];
    }
    res.removeLeadingZeros();
    res.isNegative = x.isNegative;
    return res;
}

big_integer big_integer::radix_shr(const big_integer& x, long sh) {
    big_integer res;
    size_t n = x.number.size();

    res.number.resize(n - sh);
    for (size_t i = sh; i < n; i++) {
        res.number[i - sh] = x.number[i];
    }
    res.removeLeadingZeros();
    res.isNegative = x.isNegative;
    return res;
}
