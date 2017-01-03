#include "nr.h"

// Driver for routine scrsho

DP fx(const DP x)
{
        return NR::bessj0(x);
}

int main(void)
{
        NR::scrsho(fx);
        return 0;
}
