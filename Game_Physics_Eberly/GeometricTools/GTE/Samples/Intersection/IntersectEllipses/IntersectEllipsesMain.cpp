// David Eberly, Geometric Tools, Redmond WA 98052
// Copyright (c) 1998-2025
// Distributed under the Boost Software License, Version 1.0.
// https://www.boost.org/LICENSE_1_0.txt
// https://www.geometrictools.com/License/Boost/LICENSE_1_0.txt
// Version: 6.7.2023.06.16

#include "IntersectEllipsesWindow2.h"
#include <iostream>

int main()
{
    try
    {
        Window::Parameters parameters(L"IntersectEllipsesWindow2", 0, 0, 1024, 1024);
        auto window = TheWindowSystem.Create<IntersectEllipsesWindow2>(parameters);
        TheWindowSystem.MessagePump(window, TheWindowSystem.NO_IDLE_LOOP);
        TheWindowSystem.Destroy(window);
    }
    catch (std::exception const& e)
    {
        std::cout << e.what() << std::endl;
    }
    return 0;
}
