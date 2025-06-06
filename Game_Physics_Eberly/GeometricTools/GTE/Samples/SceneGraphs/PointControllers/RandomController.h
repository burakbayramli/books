// David Eberly, Geometric Tools, Redmond WA 98052
// Copyright (c) 1998-2025
// Distributed under the Boost Software License, Version 1.0.
// https://www.boost.org/LICENSE_1_0.txt
// https://www.geometrictools.com/License/Boost/LICENSE_1_0.txt
// Version: 6.0.2022.01.06

#pragma once

#include <Graphics/PointController.h>
#include <random>

namespace gte
{
    class RandomController : public PointController
    {
    public:
        RandomController(BufferUpdater const& postUpdate);

    protected:
        virtual void UpdatePointMotion(float ctrlTime) override;

        std::default_random_engine mDRE;
        std::uniform_real_distribution<float> mURD;
    };
}
