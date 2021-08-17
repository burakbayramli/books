// Tokenizer.cpp
// "strtok" for std::string, using a table lookup 
// that only works for single byte characters
// 2007/01/21
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Tokenizer.h"

typedef unsigned char BYTE;


//---------------------------------------------------------
Tokenizer::Tokenizer(const char* cs, const char* csDelim)
//---------------------------------------------------------
  : m_cs(cs), m_nCurPos(0)
{
  SetDelimiters(csDelim);
}


//---------------------------------------------------------
void Tokenizer::SetDelimiters(const char* csDelim)
//---------------------------------------------------------
{
  size_t N = strlen(csDelim);
  for (size_t i=0; i<N; ++i)
    m_delim.set(static_cast<BYTE>(csDelim[i]));
}


//---------------------------------------------------------
int Tokenizer::Next(std::string& cs)
//---------------------------------------------------------
{
  // Note: must return signed int, not size_t

  cs.clear();

  while (m_nCurPos < m_cs.size() && m_delim[static_cast<BYTE>(m_cs.at(m_nCurPos))])
    ++m_nCurPos;

  if (m_nCurPos >= m_cs.size()) 
    return -1;

  size_t nStartPos = m_nCurPos;
  while (m_nCurPos < m_cs.size() && !m_delim[static_cast<BYTE>(m_cs.at(m_nCurPos))])
    ++m_nCurPos;

  cs = m_cs.substr(nStartPos, m_nCurPos - nStartPos);

  int nRet = (int)(nStartPos);
  return nRet;
}


//---------------------------------------------------------
std::string Tokenizer::Tail()
//---------------------------------------------------------
{
  size_t nCurPos = m_nCurPos;

  while (nCurPos < m_cs.size() && m_delim[(size_t)static_cast<BYTE>(m_cs.at(nCurPos))])
    ++nCurPos;

  std::string csResult;

  if (nCurPos < m_cs.size())
    csResult = m_cs.substr(nCurPos);

  return csResult;
}
