from typing import Self
import math

def find_period(s: str) -> int:
    return bool(s) and (s+s).find(s,1,-1) or -1

def add_from_tuple(a: tuple[int], b: tuple[int],base: int, carry:int = 0) -> tuple[tuple[int],int]:
    result = []
    for x, y in zip(reversed(a),reversed(b)):
        carry, add_digits = divmod(x+y+carry,base)
        result.append(add_digits)

    return tuple(result[::-1]), carry

def normalize_period(digits: str, rep: int) -> tuple[str,int]:
    size = len(digits)
    
    period_length = find_period(digits[rep:size])
    if period_length > 0: size = rep + period_length #simplifies a periodic period

    if size > rep:
        while digits[rep-1] == digits[size-1]: #starts the period as early as possible
            rep -= 1
            size-= 1

    digits = digits[:size]

    if all(d=='0' for d in digits[:rep]):
        period = digits[rep:]
        for d in period:
            if d == '0':
                digits += '0'
                rep += 1
                size+= 1
            else: break

    return (digits,rep)

class repfloat:
    def __init__(self,mantissa:tuple[int],i_rep:int,exponent:int,neg:bool=False) -> None:
        self.mantissa = mantissa
        self.i_rep = i_rep
        self.exponent = exponent
        self.neg = neg

    @classmethod
    def make_float(cls, numeral:str,radix:str = ',',rep='r') -> Self:
        if rep not in numeral: numeral += rep
        if radix not in numeral: numeral = numeral.replace(rep,radix+rep)
        negative = '-' in numeral
        numeral = numeral.replace('-','')
        
        stripped = numeral.translate(str.maketrans({radix:None,rep:None,'0':None}))
        first_digit = (stripped or '0')[0]
        
        i_radix = numeral.find(radix)
        i_rep = numeral.find(rep) - 1 # -1 to ignore the comma

        digits, i_rep = normalize_period(numeral.replace(radix,'').replace(rep,''),i_rep)
        i_first = digits.find(first_digit)

        digits = digits.lstrip('0') or "0"

        exponent = i_radix - i_first
        i_rep = i_rep - i_first
        
        mantissa = tuple(map(int,digits))

        return cls(mantissa,i_rep,exponent,negative)
    
    def __str__(self:Self) -> str:
        mantissa = [str(d) for d in self.mantissa]
        return f"0,{''.join(mantissa[:self.i_rep])}r{''.join(mantissa[self.i_rep:])} e{self.exponent}"

    # def set_length(self,target_len:int) -> None:        
    #     prev_len = len(self.mantissa)
    #     offset = target_len - prev_len
    #     if offset > 0:
    #         if self.i_rep == prev_len: self.mantissa += (0,)*offset
    #         else:
    #             period = self.mantissa[self.i_rep:] 
    #             self.mantissa = (self.mantissa + period * (offset//len(period) + 1))[:target_len]
            
    #         self.i_rep += offset
        
    #     elif offset <0:
    #         ...

    def match_floats(self,other:Self) -> tuple[Self,Self]:
        if self.exponent >= other.exponent:
            a,b = self, other
        else: 
            a,b = other, self
        
        if a.mantissa == (0,):
            return repfloat(b.mantissa, b.i_rep,b.exponent,b.neg), repfloat((0,) * len(b.mantissa), b.i_rep,b.exponent,a.neg)
        if b.mantissa == (0,):
            return repfloat(a.mantissa, a.i_rep,a.exponent,a.neg), repfloat((0,) * len(a.mantissa), a.i_rep,a.exponent,b.neg)

        exp = a.exponent
        m_a = a.mantissa
        m_b = (a.exponent - b.exponent) * (0,) + b.mantissa
        i_a = a.i_rep
        i_b = (a.exponent - b.exponent) + b.i_rep

        m_a += (0,) * (len(m_a) == i_a) #puts a repeating 0 at the end of a number without a period
        m_b += (0,) * (len(m_b) == i_b)

        period_a = m_a[i_a:]
        period_b = m_b[i_b:]
        offset = i_b - i_a
        if offset > 0:
            m_a += period_a * (offset // len(period_a)) + period_a[:offset % len(period_a)]
            i_a += offset
        if offset < 0:
            m_b += period_b * ((-offset) // len(period_b)) + period_b[:(-offset) % len(period_b)]
            i_b -= offset
        
        period_a = m_a[i_a:]
        period_b = m_b[i_b:]

        period_length = math.lcm(len(period_a), len(period_b))

        m_a += period_a * (period_length//len(period_a) - 1) if period_a else tuple()
        m_b += period_b * (period_length//len(period_b) - 1) if period_b else tuple()

        if m_a < m_b:
            return repfloat(m_b,i_b,exp,b.neg), repfloat(m_a,i_a,exp,a.neg)
        else:
            return repfloat(m_a,i_a,exp,a.neg), repfloat(m_b,i_b,exp,b.neg)
    
    def difference(self,other:Self,base:int) -> Self:
        a, b = self.match_floats(other)


    def __lt__(self,other:Self) -> bool:
        if self.neg and not other.neg:
            return True
        if not self.neg and other.neg:
            return False
        exp = max(self.exponent, other.exponent)
        period_a = self.mantissa[ self.i_rep:] or (0,)
        period_b =other.mantissa[other.i_rep:] or (0,)

        period_length = math.lcm(len(period_a),len(period_b))
        m_a = (0,) * (exp - self.exponent) + self.mantissa[: self.i_rep]
        m_b = (0,) * (exp -other.exponent) +other.mantissa[:other.i_rep]
        total_length = max(len(m_a),len(m_b)) + period_length
        i_rep = max(self.i_rep +exp-self.exponent, other.i_rep + exp - other.exponent)
        
        m_a+= period_a * (1+(i_rep-self.i_rep)//len(period_a) + period_length//len(period_a))
        m_b+= period_b * (1+(i_rep-other.i_rep)//len(period_b)+ period_length//len(period_b))

        lt_abs = (m_a[:total_length] < m_b[:total_length])
        # print(self,'<',other,self.neg ^ lt_abs)
        return self.neg ^ lt_abs
    
    def __eq__(self,other:Self) -> bool:
        a, b = self.match_floats(other)
        return (a.mantissa == b.mantissa) and (self.neg == other.neg)

    def __le__(self,other:Self) -> bool:
        return (self<other) or (self == other)

    
class machine:
    def __init__(self, t:int, e_min:int, e_max:int,base:int=2) -> None:
        self.base = base
        self.t = t
        self.e_min = e_min
        self.e_max = e_max

    def put_number(self, numeral:str) -> repfloat: 
        rfloat = repfloat.make_float(numeral)

        return self.round(rfloat)
    
    def round(self, x: repfloat) -> repfloat:
        prev_len = len(x.mantissa)
        offset = self.t - prev_len
        if offset > 0:
            if x.i_rep == prev_len: x.mantissa += (0,)*offset
            else:
                period = x.mantissa[x.i_rep:] 
                x.mantissa = (x.mantissa + period * (offset//len(period) + 1))[:self.t]
            x.i_rep += offset
            return x
        if offset == 0:
            return x
        
        mantissa = x.mantissa[:self.t]
        round_up,carry = add_from_tuple(mantissa,(0,) * self.t, self.base, 1)
        if carry: round_up = (carry,) + round_up[:-1]
        low = repfloat(mantissa,self.t,x.exponent)
        high= repfloat(round_up,self.t,x.exponent + bool(carry))
        for i in range(self.t):
            test = repfloat(mantissa,i,x.exponent)
            if low < test <= x:
                low = test
            elif x <= test < high:
                high = test
                carry = 0
        diff_low = self.subtract_abs(*(x.match_floats(low)))
        diff_low.neg = False
        if carry:
            m_diff = (0,) * (len(mantissa)-1) + (1,)
            diff_high = repfloat(m_diff,x.i_rep,x.exponent)
        else:
            diff_high = self.subtract_abs(*(high.match_floats(x)))            

        if diff_low <= diff_high:
            return repfloat(low.mantissa,low.i_rep,low.exponent,x.neg)
        else:
            return repfloat(high.mantissa,high.i_rep,high.exponent,x.neg)

    def add_abs(self, a:repfloat, b:repfloat) -> repfloat:
        i_rep = a.i_rep
        carry = (a.mantissa[i_rep] + b.mantissa[i_rep]) // self.base
        period, carry = add_from_tuple(a.mantissa[i_rep:], b.mantissa[i_rep:],self.base,carry)

        if all((d==self.base-1 for d in period)):
            period = (0,)
            carry += 1
        
        finite, carry = add_from_tuple(a.mantissa[:i_rep], b.mantissa[:i_rep],self.base,carry)

        mantissa = (carry,) + finite + period
        i_rep += 1

        return repfloat(mantissa, i_rep,a.exponent + 1)

    def subtract_abs(self, a:repfloat, b:repfloat) -> repfloat:
        i_rep = a.i_rep
        b_negative = tuple(-d for d in b.mantissa)
        carry = (((len(a.mantissa)-i_rep) and a.mantissa[i_rep]) + ((len(a.mantissa)-i_rep) and b_negative[i_rep])) // self.base
        period, carry = add_from_tuple(a.mantissa[i_rep:], b_negative[i_rep:],self.base,carry)

        if all((d==self.base-1 for d in period)):
            period = (0,)
            carry += 1
        
        finite, carry = add_from_tuple(a.mantissa[:i_rep], b_negative[:i_rep],self.base,carry)

        mantissa = (carry,) + finite + period
        i_rep += 1

        return repfloat(mantissa, i_rep,a.exponent + 1)

    
    def add(self, x:repfloat, y:repfloat) -> repfloat:
        a, b = repfloat.match_floats(x,y)
        if a.neg == b.neg:
            result = self.add_abs(a,b)
            result.neg = a.neg
        else:
            result = self.subtract_abs(a,b)
            result.neg = a.neg
        
        return self.round(result)
        
        

if __name__ == "__main__":
    f = machine(3,-1,2,2)
    numstrings = [
                    "0", 
                    # "0,1", 
                    "0,0001",
                    "0,00101r110",
                    # "0,00r10",
                    "0r0011",
                    # "0,r001", 
                    # "0,1r1", 
                    "1,11",
                    "111", 
                    "11r01"
                  ]
    pi = str(math.pi).replace('.',',')
    machines = [machine(i,-1,2,10) for i in range(1,10)]

    nums = [repfloat.make_float(n) for n in numstrings]
    for n in numstrings:
        print(n,f.put_number(n),sep='\t')
    # for n in nums:
    #     for m in nums:
    #         print(n,m,(n.match_floats(m)[0]),n.match_floats(m)[1],sep='\t\t')
    print(pi)
    for f in machines:
        print(f.put_number(str(pi)))

    # print(f.put_number("1101,1101r0001"))
    
    adder = machine(4,-1,1,2)
    a = adder.put_number("0r01")
    b = adder.put_number("0,1")

    x = adder.add(a,b)
    print(x)