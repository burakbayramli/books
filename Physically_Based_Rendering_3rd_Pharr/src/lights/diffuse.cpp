
/*
    pbrt source code is Copyright(c) 1998-2016
                        Matt Pharr, Greg Humphreys, and Wenzel Jakob.

    This file is part of pbrt.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

    - Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    - Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
    IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
    TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
    PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 */

// lights/diffuse.cpp*
#include "lights/diffuse.h"
#include "paramset.h"
#include "sampling.h"
#include "shapes/triangle.h"
#include "stats.h"

namespace pbrt {

// DiffuseAreaLight Method Definitions
DiffuseAreaLight::DiffuseAreaLight(const Transform &LightToWorld,
                                   const MediumInterface &mediumInterface,
                                   const Spectrum &Lemit, int nSamples,
                                   const std::shared_ptr<Shape> &shape,
                                   bool twoSided)
    : AreaLight(LightToWorld, mediumInterface, nSamples),
      Lemit(Lemit),
      shape(shape),
      twoSided(twoSided),
      area(shape->Area()) {
    // Warn if light has transformation with non-uniform scale, though not
    // for Triangles, since this doesn't matter for them.
    if (WorldToLight.HasScale() &&
        dynamic_cast<const Triangle *>(shape.get()) == nullptr)
        Warning(
            "Scaling detected in world to light transformation! "
            "The system has numerous assumptions, implicit and explicit, "
            "that this transform will have no scale factors in it. "
            "Proceed at your own risk; your image may have errors.");
}

Spectrum DiffuseAreaLight::Power() const {
    return (twoSided ? 2 : 1) * Lemit * area * Pi;
}

Spectrum DiffuseAreaLight::Sample_Li(const Interaction &ref, const Point2f &u,
                                     Vector3f *wi, Float *pdf,
                                     VisibilityTester *vis) const {
    ProfilePhase _(Prof::LightSample);
    Interaction pShape = shape->Sample(ref, u, pdf);
    pShape.mediumInterface = mediumInterface;
    if (*pdf == 0 || (pShape.p - ref.p).LengthSquared() == 0) {
        *pdf = 0;
        return 0.f;
    }
    *wi = Normalize(pShape.p - ref.p);
    *vis = VisibilityTester(ref, pShape);
    return L(pShape, -*wi);
}

Float DiffuseAreaLight::Pdf_Li(const Interaction &ref,
                               const Vector3f &wi) const {
    ProfilePhase _(Prof::LightPdf);
    return shape->Pdf(ref, wi);
}

Spectrum DiffuseAreaLight::Sample_Le(const Point2f &u1, const Point2f &u2,
                                     Float time, Ray *ray, Normal3f *nLight,
                                     Float *pdfPos, Float *pdfDir) const {
    ProfilePhase _(Prof::LightSample);
    // Sample a point on the area light's _Shape_, _pShape_
    Interaction pShape = shape->Sample(u1, pdfPos);
    pShape.mediumInterface = mediumInterface;
    *nLight = pShape.n;

    // Sample a cosine-weighted outgoing direction _w_ for area light
    Vector3f w;
    if (twoSided) {
        Point2f u = u2;
        // Choose a side to sample and then remap u[0] to [0,1] before
        // applying cosine-weighted hemisphere sampling for the chosen side.
        if (u[0] < .5) {
            u[0] = std::min(u[0] * 2, OneMinusEpsilon);
            w = CosineSampleHemisphere(u);
        } else {
            u[0] = std::min((u[0] - .5f) * 2, OneMinusEpsilon);
            w = CosineSampleHemisphere(u);
            w.z *= -1;
        }
        *pdfDir = 0.5f * CosineHemispherePdf(std::abs(w.z));
    } else {
        w = CosineSampleHemisphere(u2);
        *pdfDir = CosineHemispherePdf(w.z);
    }

    Vector3f v1, v2, n(pShape.n);
    CoordinateSystem(n, &v1, &v2);
    w = w.x * v1 + w.y * v2 + w.z * n;
    *ray = pShape.SpawnRay(w);
    return L(pShape, w);
}

void DiffuseAreaLight::Pdf_Le(const Ray &ray, const Normal3f &n, Float *pdfPos,
                              Float *pdfDir) const {
    ProfilePhase _(Prof::LightPdf);
    Interaction it(ray.o, n, Vector3f(), Vector3f(n), ray.time,
                   mediumInterface);
    *pdfPos = shape->Pdf(it);
    *pdfDir = twoSided ? (.5 * CosineHemispherePdf(AbsDot(n, ray.d)))
                       : CosineHemispherePdf(Dot(n, ray.d));
}

std::shared_ptr<AreaLight> CreateDiffuseAreaLight(
    const Transform &light2world, const Medium *medium,
    const ParamSet &paramSet, const std::shared_ptr<Shape> &shape) {
    Spectrum L = paramSet.FindOneSpectrum("L", Spectrum(1.0));
    Spectrum sc = paramSet.FindOneSpectrum("scale", Spectrum(1.0));
    int nSamples = paramSet.FindOneInt("samples",
                                       paramSet.FindOneInt("nsamples", 1));
    bool twoSided = paramSet.FindOneBool("twosided", false);
    if (PbrtOptions.quickRender) nSamples = std::max(1, nSamples / 4);
    return std::make_shared<DiffuseAreaLight>(light2world, medium, L * sc,
                                              nSamples, shape, twoSided);
}

}  // namespace pbrt
