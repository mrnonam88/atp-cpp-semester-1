#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

class BigInteger{
private:
    std::vector<long long> blocks;    // in reverse order
    long long size = 0;          //amount of blocks
    int base = 1e9;
    bool positive = true;
    int pr = 9;
    void resize() {
        for (long long i = 0; i < size; ++i) {
            if (blocks[i] >= base){
                if (i == size - 1){
                    blocks.push_back(0);
                    size++;
                }
                blocks[i + 1] += blocks[i]/base;
                blocks[i] %= base;
            }
            if (blocks[i] < 0){
                if (i == size - 1){
                    blocks[i] *= -1;
                    positive = false;
                }else{
                    blocks[i] += base;
                    blocks[i + 1]--;
                }
            }
        }
        for (long long i = size-1; i > 0 ; --i) {
            if (blocks[i] == 0){
                blocks.pop_back();
                size--;
            }else{
                i = 0;
            }
        }
        if(size == 1 && blocks[0] == 0){
            positive = true;
        }
    }
    void unsigned_sum(const BigInteger& num) {
        blocks.resize(std::max(size, num.size) + 1);
        for (long long i = 0; i < num.size; ++i) {
            blocks[i] += num.blocks[i];
            if (blocks[i] > 0) {
                blocks[i + 1]++;
                blocks[i] -= base;
            }
        }
        positive = true;
        size = std::max(size, num.size) + 1;
    }
    void new_unsigned_diff(const BigInteger& num) {
        blocks.resize(std::max(size, num.size));
        for (long long i = 0; i < num.size; ++i) {
            blocks[i] -= num.blocks[i];
            if (blocks[i] < 0 && i != num.size - 1) {
                blocks[i] += base;
                blocks[i + 1]--;
            }
        }
        positive = true;
        size = std::max(size, num.size);
        if (blocks[size - 1] < 0) {
            positive = false;
            blocks[size - 1] *= -1;
            for (long long i = size - 2; i >= 0; --i) {
                blocks[i] = base - blocks[i];
                blocks[i + 1]--;
            }
        }
    }
    std::vector<long long> unsigned_division(const BigInteger& num) {
        std::vector<long long> carry(num.size);
        std::vector<long long> result(size);
        std::vector<long long> count_arr;
        long long upper_bound = 0;
        long long bottom_bound = 0;
        long long current_bound = 0;
        for (long long i = 0; i < size; ++i) {
            carry.insert(carry.begin(), blocks[size - 1 - i]);
            carry[carry.size() - 2] += carry[carry.size() - 1]*base;
            carry.pop_back();
            //quick check
            upper_bound = carry[carry.size() - 1] / num.blocks[num.size - 1];
            bottom_bound = carry[carry.size() - 1] / (num.blocks[num.size - 1] + 1);
            current_bound = (upper_bound + bottom_bound)/2;
            while(upper_bound != bottom_bound) {
                count_arr = carry;
                for (long long j = 0; j < num.size; ++j) {
                    count_arr[j] -= current_bound * num.blocks[j];
                    if (count_arr[j] < 0) {
                        if (j != num.size - 1) {
                            count_arr[j + 1] -= std::abs(count_arr[j]) / base;
                            if (std::abs(count_arr[j]) % base) count_arr[j + 1]--;
                            count_arr[j] %= base;
                            count_arr[j] += base;
                        }
                    }
                }
                if (count_arr[num.size - 1] < 0) {
                    if (upper_bound - bottom_bound == 1) upper_bound = bottom_bound;
                    else {
                        upper_bound = current_bound;
                        current_bound = (bottom_bound + upper_bound) / 2;
                    }
                } else {
                    if (upper_bound - bottom_bound == 1) {
                        if (current_bound == upper_bound) bottom_bound = upper_bound;
                        else current_bound = upper_bound;
                    }
                    else {
                        bottom_bound = current_bound;
                        current_bound = (bottom_bound + upper_bound) / 2;
                    }
                }
            }
            result[i] = bottom_bound;
            for (long long j = 0; j < num.size; ++j) {
                carry[j] -= bottom_bound * num.blocks[j];
                if (carry[j] < 0) {
                    carry[j + 1] -= std::abs(carry[j]) / base;
                    if (std::abs(carry[j]) % base != 0) carry[j + 1]--;
                    carry[j] %= base;
                    carry[j] += base;
                }
            }
        }
        std::reverse(result.begin(), result.end());
        blocks = result;
        size = result.size();
        return  carry;
    }
public:
    //Constructors
    BigInteger() = default;
    BigInteger(long long num) {
        size = 1;
        blocks = std::vector<long long>(size);
        positive = num >= 0;
        blocks[0] = std::abs(num);
    }
    BigInteger(const std::string& num) {
        if (num[0] == '-'){
            positive = false;
        }
        long long in_size = num.size() - 1 + positive;
        size = in_size/pr + 1;
        blocks = std::vector<long long>(size);
        int counter = 0;
        for (long long i = num.size() - 1; i >= 1 - positive; i -= pr) {
            if (i >= pr-1 + 1-positive) {
                blocks[counter] = std::stoi(num.substr(i - pr + 1, pr));
                counter ++;
            }else {
                blocks[counter] = std::stoi(num.substr(1 - positive, i + positive));
            }
        }
        resize();
    }
//======================================================================================================================
    //Operators
    BigInteger& operator+=(const BigInteger& num) {
        bool old_sign = positive;
        if (positive == num.positive) {
            unsigned_sum(num);
            positive = old_sign;
        }else {
            new_unsigned_diff(num);
            if (!old_sign){
                positive = !positive;
            }
        }
        resize();
        return *this;
    }
    BigInteger& operator-=(const BigInteger& num) {
        bool old_sign = positive;
        if (positive == num.positive) {
            new_unsigned_diff(num);
            if (!old_sign) {
                positive = !positive;
            }
        }else {
            unsigned_sum(num);
            positive = old_sign;
        }
        resize();
        return *this;
    }
    BigInteger& operator*=(const BigInteger& num) {
        std::vector<long long> copy(size + num.size);
        for (long long i = 0; i < size; ++i) {
            for (int j = 0; j < num.size; ++j) {
                copy[i + j] += blocks[i] * num.blocks[j];
                if (copy[i + j] > base) {
                    copy[i + j + 1] += copy[i + j] / base;
                    copy[i + j] %= base;
                }
            }
        }
        positive = !(positive ^ num.positive);
        blocks = copy;
        size += num.size;
        resize();
        return  *this;
    }
    BigInteger& operator/=(const BigInteger& num) {
        unsigned_division(num);
        positive = !(positive ^ num.positive);
        resize();
        return *this;
    }
    BigInteger& operator%=(const BigInteger& num) {
        blocks = unsigned_division(num);
        size = blocks.size();
        resize();
        return *this;
    }
    BigInteger&  operator++() {
        *this += 1;
        return *this;
    }
    BigInteger&  operator--() {
        *this -= 1;
        return *this;
    }
    BigInteger  operator++(int) {
        BigInteger copy = *this;
        ++(*this);
        return copy;
    }
    BigInteger  operator--(int) {
        BigInteger copy = *this;
        --(*this);
        return copy;
    }

    //Implicit conversions
    explicit operator int() {
        long long result = 0;
        if (size > 1) result += blocks[1];
        if (size > 0) result += blocks[0] * base;
      return int(result);
    }
    explicit operator long long() {
        return blocks[0];
    }
    explicit operator bool() {
        bool sooth = false;
        if (positive && size > 0 && blocks[0] > 0) sooth = true;
        return sooth;
    }
    friend bool operator<(const BigInteger& num_1, const BigInteger& num_2);
//======================================================================================================================
    //Methods
    void clear() {
        blocks.clear();
        size = 0;
    }
    void debug(const std::string& x = "", bool quick = true) const {
        if (quick){
            std::cerr << "O-> " << x << "  ";
            std::cerr << "N-> ";
            if (positive) std::cerr << '+';
            else std::cerr << '-';
            for (long long i = size - 1; i >= 0 ; --i) {
                std::cerr << blocks[i] << " ";
            }
            std::cerr << '\n';
            return;
        }
        for (int i = 0; i < 20; ++i) {
            std::cout << "=";
        }
        std::cout << "\n";
        if (x != "") std::cout << "OPERATION -> " << x << '\n';
        std::cout << "DEBUG BigInteger\n";
        std::cout << "SIZE -> " << size << "\n";
        std::cout << "SIGN -> ";
        if (positive) std::cout << '+' << "\n";
        else std::cout << '-' << "\n";
        std::cout << "NUMBER -> ";
        for (long long i = size - 1; i >= 0 ; --i) {
            std::cout << blocks[i] << " ";
        }
        std::cout << "\nSAVED AS -> ";
        for (long long i = 0; i < size ; ++i) {
            std::cout << blocks[i] << " ";
        }
        std::cout << "\n";
        for (int i = 0; i < 20; ++i) {
            std::cout << "=";
        }
        std::cout << "\n";
    }
    [[nodiscard]] std::string toString() const {
        std::string output;
        if (!positive) {
            output.push_back('-');
        }
        for (long long i = size - 1; i >= 0; --i) {
            auto number = std::to_string(blocks[i]);
            if(i == size - 1) {
                output += number;
            }else {
                output += std::string(9 - number.length(), '0') + number;
            }
        }
        return output;
    }
    [[nodiscard]] BigInteger abs() const {
        BigInteger copy = *this;
        copy.positive = true;
        return copy;
    }
};
//**********************************************************************************************************************
//Operators
BigInteger operator+(const BigInteger& num_1, const BigInteger& num_2) {
    BigInteger copy = num_1;
    copy += num_2;
    return copy;
}
BigInteger operator-(const BigInteger& num_1, const BigInteger& num_2) {
    BigInteger copy = num_1;
    copy -= num_2;
    return copy;
}
BigInteger operator*(const BigInteger& num_1, const BigInteger& num_2) {
    BigInteger copy = num_1;
    copy *= num_2;
    return copy;
}
BigInteger operator/(const BigInteger& num_1, const BigInteger& num_2) {
    BigInteger copy = num_1;
    copy /= num_2;
    return copy;
}
BigInteger operator%(const BigInteger& num_1, const BigInteger& num_2) {
    BigInteger copy = num_1;
    copy %= num_2;
    return copy;
}
BigInteger operator-(const BigInteger& num) {
    BigInteger copy = num;
    copy *= -1;
    return copy;
}
//Logic operators
bool operator<(const BigInteger& num_1, const BigInteger& num_2) {
    int result = 0;
    // 0 - less
    //1 - more
    //2 - equal
    if (num_1.positive && !num_2.positive) {
        result = 1;
    }else if (!num_1.positive && num_2.positive) {
        result = 0;
    }else {
        if (num_2.size < num_1.size) {
            result = 1;
        }
        else if (num_2.size == num_1.size) {
            for (long long i = num_1.size - 1; i >= 0; --i) {
                if (num_1.blocks[i] > num_2.blocks[i]) {
                    result = 1;
                }else if (num_1.blocks[i] < num_2.blocks[i]) {
                    result = 0;
                }else{
                    result = 2;
                }
            }
        }
        if (result != 2){
            if (!num_1.positive){
                result += 1;
                result %= 2;
            }
        }
    }
    if (result == 0) return true;
    return false;
}
bool operator>(const BigInteger& num_1, const BigInteger& num_2) {
    return num_2<num_1;
}
bool operator==(const BigInteger& num_1, const BigInteger& num_2) {
    return !((num_2 < num_1) || (num_1 < num_2));
}
bool  operator!=(const BigInteger& num_1, const BigInteger& num_2) {
    return !(num_1 == num_2);
}
bool  operator<=(const BigInteger& num_1, const BigInteger& num_2) {
    return (num_1 < num_2) || (num_1 == num_2);
}
bool  operator>=(const BigInteger& num_1, const BigInteger& num_2) {
    return (num_1 > num_2) || (num_1 == num_2);
}
//Output operator
std::ostream& operator<<(std::ostream& output, const BigInteger& biggie) {
    output << biggie.toString();
    return output;
}
//Input operator
std::istream& operator>>(std::istream& input, BigInteger& biggie) {
    input.tie(nullptr);
    biggie.clear();
    std::string input_num;
    input >> input_num;
    biggie = BigInteger(input_num);
    return input;
}
//######################################################################################################################
//######################################################################################################################
class Rational {
private:
    BigInteger numerator;
    BigInteger denominator = 1;
    //Methods
    BigInteger gcd(const BigInteger& num_1, const BigInteger& num_2){
        if(num_2 == 0) return num_1;
        return gcd(num_2, num_1%num_2);
    }
    void shrink(){
        if (denominator != 1) {
            BigInteger gcf = gcd(numerator.abs(), denominator.abs());
            if (gcf != 1) {
                numerator /= gcf;
                denominator /= gcf;
            }
        }
    }
public:
    //Constructors
    Rational() = default;
    Rational(const BigInteger& num): numerator(num){}
    Rational(int num): numerator(num){}
//======================================================================================================================
    //Operators
    Rational& operator+=(const Rational& num){
        numerator *= num.denominator;
        numerator += num.numerator * denominator;
        denominator *= num.denominator;
        shrink();
        return *this;
    }
    Rational& operator-=(const Rational& num){
        numerator *= num.denominator;
        numerator -= num.numerator * denominator;
        denominator *= num.denominator;
        shrink();
        return *this;
    }
    Rational& operator*=(const Rational& num){
        numerator *= num.numerator;
        denominator *= num.denominator;
        shrink();
        return *this;
    }
    Rational& operator/=(const Rational& num){
        numerator *= num.denominator;
        if (num.numerator < 0){
            numerator *= -1;
            denominator *= (-num.numerator);
        }else{
            denominator *= num.numerator;
        }
        shrink();
        return *this;
    }
    friend bool operator<(const Rational& num_1, const Rational& num_2);
    explicit operator double(){
        std::string output = asDecimal(20);
        return std::atof(output.c_str());
    }
//======================================================================================================================
    //Methods
    std::string toString(){
        std::string output = numerator.toString();
        if (denominator != 1) {
            output.push_back('/');
            output += denominator.toString();
        }
        return output;
    }
    std::string asDecimal(size_t precision = 0)  {
        std::string precised;
        BigInteger copy = numerator;
        if (numerator*denominator < 0 && copy/denominator == 0) precised += '-';
        precised += (copy/denominator).toString();
        copy = copy.abs();
        copy %= denominator;
        for (size_t i = 0; i <= precision; ++i) {
            if (i == 0) precised.push_back('.');
            else {
                copy *= 10;
                precised += (copy/denominator).toString();
                copy %= denominator;
            }
        }
        return precised;
    }
    void debug() const{
        std::cerr << '\n';
        for (int i = 0; i < 20; ++i) {
            std::cerr << '&';
        }
        std::cerr << "\n";
        std::cerr << "DEBUG RATIONAL\n";
        std::cerr << "NUMBER -> ";
        std::cerr << numerator;
        std::cerr << "\nAND BASE -> ";
        std::cerr << denominator;
        std::cerr << "\n";
        for (int i = 0; i < 20; ++i) {
            std::cerr << '&';
        }
        std::cerr << "\n";
    }

};
//**********************************************************************************************************************
//Operators
Rational operator+(const Rational& num_1, const Rational& num_2) {
    Rational copy = num_1;
    copy += num_2;
    return copy;
}
Rational operator-(const Rational& num_1, const Rational& num_2) {
    Rational copy = num_1;
    copy -= num_2;
    return copy;
}
Rational operator*(const Rational& num_1, const Rational& num_2) {
    Rational copy = num_1;
    copy *= num_2;
    return copy;
}
Rational operator/(const Rational& num_1, const Rational& num_2) {
    Rational copy = num_1;
    copy /= num_2;
    return copy;
}
Rational operator-(const Rational& num_1) {
    Rational copy = num_1;
    copy *= -1;
    return copy;

}
//Logical operators
bool operator<(const Rational& num_1, const Rational& num_2) {
    return num_1.numerator * num_2.denominator < num_2.numerator * num_1.denominator;
}
bool operator>(const Rational& num_1, const Rational& num_2) {
    return num_2<num_1;
}
bool operator==(const Rational& num_1, const Rational& num_2) {
    return !(num_2<num_1 && num_1 < num_2);
}
bool operator!=(const Rational& num_1, const Rational& num_2) {
    return !(num_1 == num_2);
}
bool operator <=(const Rational& num_1, const Rational& num_2) {
    return !(num_1 > num_2);
}
bool operator >=(const Rational& num_1, const Rational& num_2) {
    return !(num_1 < num_2);
}