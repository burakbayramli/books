// Tokenizer.h
// "strtok" for std::string, using a table lookup 
// that only works for single byte characters
// 2007/01/21
//---------------------------------------------------------
#ifndef NDG__Tokenizer_H__INCLUDED
#define NDG__Tokenizer_H__INCLUDED

#include <string>
#include <bitset>


//---------------------------------------------------------
class Tokenizer
//---------------------------------------------------------
{
public:
  Tokenizer(const char* cs, const char* csDelim);
  void SetDelimiters(const char* csDelim);

  // Note: must return signed int, not size_t
  int Next(std::string& cs);

  std::string Tail();
  size_t GetPosition() { return m_nCurPos; }

private:
  std::string       m_cs;
  std::bitset<256>  m_delim;
  size_t            m_nCurPos;
};


#endif  // NDG__Tokenizer_H__INCLUDED
