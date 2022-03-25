MODULE main
!
INTERFACE
!
SUBROUTINE bandred(a,d,e)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN OUT)::a(:,:)
 REAL(iwp),INTENT(OUT)::d(0:),e(0:)
END SUBROUTINE bandred
!
FUNCTION bandwidth(g) RESULT(nband)
 IMPLICIT NONE
 INTEGER,INTENT(IN)::g(:)
 INTEGER::nband
END FUNCTION bandwidth
!
SUBROUTINE banmul(kb,loads,ans)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::kb(:,:),loads(0:)
 REAL(iwp),INTENT(OUT)::ans(0:)
END SUBROUTINE banmul
!
SUBROUTINE bantmul(kb,loads,ans)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::kb(:,:),loads(0:)
 REAL(iwp),INTENT(OUT)::ans(0:)
END SUBROUTINE bantmul
!
SUBROUTINE beamdis(loads,nf,ratmax,interp,nels,ell,argv,nlen,ips)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::loads(0:),ratmax,ell(:)
 INTEGER,INTENT(IN)::ips,nf(:,:),nels,interp,nlen
 CHARACTER(LEN=15)::argv
END SUBROUTINE beamdis
!
SUBROUTINE beam_gm(gm,ell)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::ell
 REAL(iwp),INTENT(OUT)::gm(:,:)
END SUBROUTINE beam_gm
!
SUBROUTINE beam_km(km,ei,ell)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::ei,ell
 REAL(iwp),INTENT(OUT)::km(:,:)
END SUBROUTINE beam_km
!
SUBROUTINE beam_mm(mm,fs,ell)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::fs,ell
 REAL(iwp),INTENT(OUT)::mm(:,:)
END SUBROUTINE beam_mm
!
SUBROUTINE beemat(bee,deriv)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::deriv(:,:)
 REAL(iwp),INTENT(OUT)::bee(:,:)
END SUBROUTINE beemat
!
SUBROUTINE bee8(bee,coord,xi,eta,det)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::coord(:,:),xi,eta
 REAL(iwp),INTENT(OUT)::bee(:,:),det
END SUBROUTINE bee8
!
SUBROUTINE bisect(d,e,acheps,ifail)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::acheps
 REAL(iwp),INTENT(IN OUT)::d(0:),e(0:)
 INTEGER,INTENT(IN OUT)::ifail
END SUBROUTINE bisect
!
SUBROUTINE bmat_nonaxi(bee,radius,coord,deriv,fun,iflag,lth)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::deriv(:,:),fun(:),coord(:,:)
 REAL(iwp),INTENT(OUT)::bee(:,:),radius
 INTEGER,INTENT(IN)::iflag,lth
END SUBROUTINE bmat_nonaxi
!
SUBROUTINE checon(loads,oldlds,tol,converged)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::loads(0:),tol
 REAL(iwp),INTENT(IN OUT)::oldlds(0:)
 logical,INTENT(OUT)::CONVERGED
END SUBROUTINE checon
!
subroutine chobk1(kb,loads)
 implicit none
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),intent(in)::kb(:,:)
 REAL(iwp),intent(in out)::loads(0:)
end subroutine chobk1
!
subroutine chobk2(kb,loads)
!Choleski back-substitution
 implicit none
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),intent(in)::kb(:,:)
 REAL(iwp),intent(in out)::loads(0:)
end subroutine chobk2
!
subroutine cholin(kb)
 implicit none
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),intent(in out)::kb(:,:)
end subroutine cholin
!
SUBROUTINE comred(bk,n)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 COMPLEX(iwp),INTENT(IN OUT)::bk(:)
 INTEGER,INTENT(IN)::n
END SUBROUTINE COMRED
!
SUBROUTINE COMSUB(bk,loads)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 COMPLEX(iwp),INTENT(IN)::bk(:)
 COMPLEX(iwp),INTENT(IN OUT)::loads(0:)
END SUBROUTINE comsub
!
SUBROUTINE contour(loads,g_coord,g_num,ned,argv,nlen,ips)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::loads(0:),g_coord(:,:)
 INTEGER,INTENT(IN)::g_num(:,:),ned,ips,nlen
 CHARACTER(*),INTENT(IN)::argv
END SUBROUTINE contour
!
SUBROUTINE cross_product(b,c,a)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::b(:),c(:)
 REAL(iwp),INTENT(OUT)::a(:,:)
END SUBROUTINE cross_product
!
SUBROUTINE deemat(dee,e,v)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::e,v
 REAL(iwp),INTENT(OUT)::dee(:,:)
END SUBROUTINE deemat
!
FUNCTION determinant(jac)RESULT(det)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::jac(:,:)
 REAL(iwp)::det
END FUNCTION determinant
!
SUBROUTINE dismsh(loads,nf,ratmax,g_coord,g_num,argv,nlen,ips)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::g_coord(:,:),loads(0:),ratmax
 CHARACTER(*),INTENT(IN)::argv
 INTEGER,INTENT(IN)::g_num(:,:),ips,nf(:,:),nlen
END SUBROUTINE dismsh
!
SUBROUTINE dismsh_ensi(argv,nlen,step,nf,loads)
 IMPLICIT none
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(IN)::nlen,step,nf(:,:)
 REAL(iwp),INTENT(IN)::loads(:)
 CHARACTER(LEN=15),INTENT(IN)::argv
END SUBROUTINE dismsh_ensi
!
SUBROUTINE ecmat(ecm,fun,ndof,nodof)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::fun(:)
 REAL(iwp),INTENT(OUT)::ecm(:,:)
 INTEGER,INTENT(IN)::nodof,ndof
END SUBROUTINE ecmat
!
SUBROUTINE elmat(area,rho,emm)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::area,rho
 REAL(iwp),INTENT(OUT)::emm(:,:)
END SUBROUTINE elmat
!
SUBROUTINE exc_nods(noexe,exele,g_num,totex,ntote,nf)
 IMPLICIT NONE
 INTEGER,INTENT(IN)::noexe,exele(:),g_num(:,:)
 INTEGER,INTENT(IN OUT)::totex(:),ntote,nf(:,:)
END SUBROUTINE exc_nods
!
SUBROUTINE fkdiag(kdiag,g)
 IMPLICIT NONE
 INTEGER,INTENT(IN)::g(:)
 INTEGER,INTENT(OUT)::kdiag(:)
END SUBROUTINE fkdiag
!
SUBROUTINE fmacat(vmfl,acat)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::vmfl(:)
 REAL(iwp),INTENT(OUT)::acat(:,:)
END SUBROUTINE fmacat
!
SUBROUTINE fmdsig(dee,e,v)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::e,v
 REAL(iwp),INTENT(OUT)::dee(:,:)
END SUBROUTINE fmdsig
!
SUBROUTINE fmkdke(km,kp,c,ke,kd,theta)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::km(:,:),kp(:,:),c(:,:),theta
 REAL(iwp),INTENT(OUT)::ke(:,:),kd(:,:)
END SUBROUTINE fmkdke
!
SUBROUTINE fmplat(d2x,d2y,d2xy,points,aa,bb,i)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::points(:,:),aa,bb
 REAL(iwp),INTENT(OUT)::d2x(:),d2y(:),d2xy(:)
 INTEGER,INTENT(IN)::i
END SUBROUTINE fmplat
!
SUBROUTINE fmrmat(vmfl,dsbar,dlam,dee,rmat)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::vmfl(:),dsbar,dlam,dee(:,:)
 REAL(iwp),INTENT(OUT)::rmat(:,:)
END SUBROUTINE fmrmat
!
SUBROUTINE formaa(vmfl,rmat,daatd)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::vmfl(:),rmat(:,:)
 REAL(iwp),INTENT(OUT)::daatd(:,:)
END SUBROUTINE formaa
!
SUBROUTINE formkb(kb,km,g)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::km(:,:)
 REAL(iwp),INTENT(OUT)::kb(:,:)
 INTEGER,INTENT(IN)::g(:)
END SUBROUTINE formkb
!
SUBROUTINE formkc(bk,km,cm,g,n)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::km(:,:),cm(:,:)
 COMPLEX(iwp),INTENT(OUT)::bk(:)
 INTEGER,INTENT(IN)::g(:),n
END SUBROUTINE FORMKC
!
SUBROUTINE formke(km,kp,c,ke,theta)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::km(:,:),kp(:,:),c(:,:),theta
 REAL(iwp),INTENT(OUT)::ke(:,:)
END SUBROUTINE formke
!
SUBROUTINE formku(ku,km,g)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::km(:,:)
 REAL(iwp),INTENT(OUT)::ku(:,:)
 INTEGER,INTENT(IN)::g(:)
END SUBROUTINE formku
!
SUBROUTINE formlump(diag,emm,g)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::emm(:,:)
 REAL(iwp),INTENT(OUT)::diag(0:)
 INTEGER,INTENT(IN)::g(:)
END SUBROUTINE formlump
!
SUBROUTINE formm(stress,m1,m2,m3)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::stress(:)
 REAL(iwp),INTENT(OUT)::m1(:,:),m2(:,:),m3(:,:)
END SUBROUTINE formm
!
SUBROUTINE formtb(pb,km,g)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::km(:,:)
 INTEGER,INTENT(IN)::g(:)
 REAL(iwp),INTENT(OUT)::pb(:,:)
END SUBROUTINE formtb
!
SUBROUTINE formupv(ke,c11,c12,c21,c23,c32)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::c11(:,:),c21(:,:),c23(:,:),c32(:,:),c12(:,:)
 REAL(iwp),INTENT(OUT)::ke(:,:)
END SUBROUTINE formupv
!
SUBROUTINE form_s(gg,ell,kappa,omega,gamma,s)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::gg(:,:),kappa
 INTEGER,INTENT(IN)::ell
 REAL(iwp),INTENT(OUT)::omega,gamma(:),s(:)
END SUBROUTINE form_s
!
SUBROUTINE fsparv(kv,km,g,kdiag)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(IN)::g(:),kdiag(:)
 REAL(iwp),INTENT(IN)::km(:,:)
 REAL(iwp),INTENT(OUT)::kv(:)
END SUBROUTINE fsparv
!
SUBROUTINE gauss_band(pb,work)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN OUT)::pb(:,:),work(:,:)
END SUBROUTINE gauss_band
!
SUBROUTINE getname(argv,nlen)
 IMPLICIT NONE
 INTEGER::narg
 INTEGER,INTENT(OUT)::nlen
 CHARACTER(*),INTENT(OUT)::argv
 INTEGER::lnblnk,iargc
END SUBROUTINE getname
!
SUBROUTINE glob_to_axial(axial,global,coord)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::global(:),coord(:,:)
 REAL(iwp),INTENT(OUT)::axial
END SUBROUTINE glob_to_axial
!
SUBROUTINE glob_to_loc(local,global,gamma,coord)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::global(:),gamma,coord(:,:)
 REAL(iwp),INTENT(OUT)::local(:)
END SUBROUTINE glob_to_loc
!
SUBROUTINE hinge(coord,holdr,action,react,prop,iel,etype,gamma)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::holdr(:,:),coord(:,:),action(:),prop(:,:),gamma(:)
 REAL(iwp),INTENT(OUT)::react(:)
 INTEGER,INTENT(IN)::etype(:),iel
END SUBROUTINE hinge
!
SUBROUTINE invar(stress,sigm,dsbar,theta)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::stress(:)
 REAL(iwp),INTENT(OUT),OPTIONAL::sigm,dsbar,theta
END SUBROUTINE invar
!
SUBROUTINE interp(k,dtim,rt,rl,al,nstep)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::dtim,rt(:),rl(:)
 INTEGER,INTENT(IN)::k,nstep
 REAL(iwp),INTENT(IN OUT)::al(:,:)
END SUBROUTINE interp
!
SUBROUTINE invert(matrix)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN OUT)::matrix(:,:)
END SUBROUTINE invert
!
SUBROUTINE linmul_sky(kv,disps,loads,kdiag)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::kv(:),disps(0:)
 REAL(iwp),INTENT(OUT)::loads(0:)
 INTEGER,INTENT(IN)::kdiag(:)
END SUBROUTINE linmul_sky
!
INTEGER FUNCTION lnblnk( str )
 CHARACTER*(*),INTENT(IN)::str
END FUNCTION lnblnk
!
SUBROUTINE load_function(lf,dtim,al)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::lf(:,:),dtim
 REAL(iwp),INTENT(IN OUT)::al(:)
END SUBROUTINE load_function
!
SUBROUTINE loc_to_glob(local,global,gamma,coord)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::local(:),gamma,coord(:,:)
 REAL(iwp),INTENT(OUT)::global(:)
END SUBROUTINE loc_to_glob
!
SUBROUTINE mesh(g_coord,g_num,argv,nlen,ips)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::g_coord(:,:)
 INTEGER,INTENT(IN)::g_num(:,:),ips,nlen
 CHARACTER(*),INTENT(IN)::argv
END SUBROUTINE mesh
!
SUBROUTINE mesh_ensi(argv,nlen,g_coord,g_num,element,etype,nf,loads,     &
                     nstep,npri,dtim,solid)
 IMPLICIT none
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(IN)::nlen,nstep,npri
 INTEGER,INTENT(IN)::g_num(:,:),etype(:),nf(:,:)
 REAL(iwp),INTENT(IN)::g_coord(:,:),loads(:),dtim
 CHARACTER(LEN=15),INTENT(IN)::argv,element
 LOGICAL,INTENT(IN)::solid
END SUBROUTINE mesh_ensi
!
SUBROUTINE mcdpl(phi,psi,dee,stress,pl)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::stress(:),dee(:,:),phi,psi
 REAL(iwp),INTENT(OUT)::pl(:,:)
END SUBROUTINE mcdpl
!
SUBROUTINE mocouf(phi,c,sigm,dsbar,theta,f)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::phi,c,sigm,dsbar,theta
 REAL(iwp),INTENT(OUT)::f
END SUBROUTINE mocouf
!
SUBROUTINE mocouq(psi,dsbar,theta,dq1,dq2,dq3)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::psi,dsbar,theta
 REAL(iwp),INTENT(OUT)::dq1,dq2,dq3
END SUBROUTINE mocouq
!
FUNCTION norm(x)RESULT(l2n)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::x(:)
 REAL(iwp)::l2n
END FUNCTION norm
!
SUBROUTINE num_to_g(num,nf,g)
 IMPLICIT NONE
 INTEGER,INTENT(IN)::num(:),nf(:,:)
 INTEGER,INTENT(OUT)::g(:)
END SUBROUTINE num_to_g
!
SUBROUTINE pin_jointed(km,ea,coord)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::ea,coord(:,:)
 REAL(iwp),INTENT(OUT)::km(:,:)
END SUBROUTINE pin_jointed
!
SUBROUTINE pmsh_ensi(argv,nlen,step,loads)
 IMPLICIT none
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(IN)::nlen,step
 REAL(iwp),INTENT(IN)::loads(:)
 CHARACTER(LEN=15),INTENT(IN)::argv
END SUBROUTINE pmsh_ensi
!
SUBROUTINE rect_km(km,coord,e,v)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::coord(:,:),e,v
 REAL(iwp),INTENT(OUT)::km(:,:)
END SUBROUTINE rect_km
!
SUBROUTINE rigid_jointed(km,prop,gamma,etype,iel,coord)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::gamma(:),coord(:,:),prop(:,:)
 INTEGER,INTENT(IN)::etype(:),iel
 REAL(iwp),INTENT(OUT)::km(:,:)
END SUBROUTINE rigid_jointed
!
SUBROUTINE rod_km(km,ea,length)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::ea,length
 REAL(iwp),INTENT(OUT)::km(:,:)
END SUBROUTINE rod_km
!
SUBROUTINE rod_mm(mm,length)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),intent(in)::length
 REAL(iwp),intent(out)::mm(:,:)
END SUBROUTINE rod_mm
!
SUBROUTINE sample(element,s,wt)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(OUT)::s(:,:)
 REAL(iwp),INTENT(OUT),optional::wt(:)
 CHARACTER(*),INTENT(IN)::element
END SUBROUTINE sample
!
SUBROUTINE seep4(kp,coord,perm)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::coord(:,:),perm(:,:)
 REAL(iwp),INTENT(OUT)::kp(:,:)
END SUBROUTINE seep4
!
SUBROUTINE shape_der(der,points,i)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(IN)::i
 REAL(iwp),INTENT(IN)::points(:,:)
 REAL(iwp),INTENT(OUT)::der(:,:)
END SUBROUTINE shape_der
!
SUBROUTINE shape_fun(fun,points,i)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(in)::i
 REAL(iwp),INTENT(IN)::points(:,:)
 REAL(iwp),INTENT(OUT)::fun(:)
END SUBROUTINE shape_fun
!
SUBROUTINE solve_band(pb,work,loads)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::pb(:,:),work(:,:)
 REAL(iwp),INTENT(OUT)::loads(0:)
END SUBROUTINE solve_band
!
SUBROUTINE spabac(kv,loads,kdiag)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::kv(:)
 REAL(iwp),INTENT(IN OUT)::loads(0:)
 INTEGER,INTENT(IN)::kdiag(:)
END SUBROUTINE spabac
!
SUBROUTINE spabac_gauss(kv,loads,kdiag)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::kv(:)
 REAL(iwp),INTENT(IN OUT)::loads(0:)
 INTEGER,INTENT(IN)::kdiag(:)
END SUBROUTINE spabac_gauss
!
SUBROUTINE sparin(kv,kdiag)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN OUT)::kv(:)
 INTEGER,INTENT(IN)::kdiag(:)
END SUBROUTINE sparin
!
SUBROUTINE sparin_gauss(kv,kdiag)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(IN)::kdiag(:)
 REAL(iwp),INTENT(OUT)::kv(:)
END SUBROUTINE sparin_gauss
!
SUBROUTINE stability(kv,gv,kdiag,tol,limit,iters,evec,eval)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(IN)::limit,kdiag(:)
 INTEGER,INTENT(OUT)::iters
 REAL(iwp),INTENT(IN OUT)::kv(:),gv(:),tol,eval
 REAL(iwp),INTENT(OUT)::evec(:)
END SUBROUTINE stability
!
SUBROUTINE stiff3(km,coord,ym,pr)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::coord(:,:),ym,pr
 REAL(iwp),INTENT(OUT)::km(:,:)
END SUBROUTINE stiff3
!
SUBROUTINE stiff4(km,coord,ym,pr)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::coord(:,:),ym,pr
 REAL(iwp),INTENT(OUT)::km(:,:)
END SUBROUTINE stiff4
!
SUBROUTINE stiff6(km,coord,ym,pr)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::coord(:,:),ym,pr
 REAL(iwp),INTENT(OUT)::km(:,:)
END SUBROUTINE stiff6
!
SUBROUTINE stiff10(km,coord,ym,pr)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::coord(:,:),ym,pr
 REAL(iwp),INTENT(OUT)::km(:,:)
END SUBROUTINE stiff10
!
SUBROUTINE stiff15(km,coord,ym,pr)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::coord(:,:),ym,pr
 REAL(iwp),INTENT(OUT)::km(:,:)
END SUBROUTINE stiff15
!
SUBROUTINE vecmsh(loads,nf,ratmax,cutoff,g_coord,g_num,argv,nlen,ips)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::g_coord(:,:),loads(0:),ratmax,cutoff
 INTEGER,INTENT(IN)::g_num(:,:),ips,nf(:,:),nlen
 CHARACTER(*),INTENT(IN)::argv
END SUBROUTINE vecmsh
!
SUBROUTINE vmdpl(dee,stress,pl)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::stress(:),dee(:,:)
 REAL(iwp),INTENT(OUT)::pl(:,:)
END SUBROUTINE vmdpl
!
SUBROUTINE vmflow(stress,dsbar,vmfl)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::stress(:),dsbar
 REAL(iwp),INTENT(OUT)::vmfl(:)
END SUBROUTINE vmflow
!
END INTERFACE
!
END MODULE main
