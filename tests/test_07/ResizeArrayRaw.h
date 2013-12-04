/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

/*
   ResizeArrayRaw template
   Object Requirements: new
                        Elem(Elem &)
  			~Elem()
                        Elem & operator= (Elem &)
*/


#ifndef RESIZEARRAYRAW_H
#define RESIZEARRAYRAW_H

#include <new>
#include <string.h>

#define GrowthFactor 1.5
#define MinSize 8

// Need this juju to use templated friend below
template <class Type> class ResizeArray;
template <class Type> class SortableResizeArray;
template <class Type> class ResizeArrayIter;

// Class assumes that one can bit move objects
// around on array.  This will be true
// as long as object has no pointers to itself.
template <class Elem> class ResizeArrayRaw {

  private:
    Elem *array;
    unsigned char *varray;

    int arraySize;
    int allocSize;

    int refCount;

    float growthFactor;
    int minSize;

    // No constructor run on new elements
    // arraySize is not adjusted, only allocSize
    void resizeRaw(int size) {
      if (size <= allocSize) return;
  
      if (size < (int)(allocSize*growthFactor)) 
        size = (int)(allocSize*growthFactor);
      if ( (size-allocSize) < minSize) 
        size = allocSize+minSize;

      // align everything to 32-byte boundaries (if possible)
      unsigned char *tmpv = new unsigned char[size*sizeof(Elem)+31];
      //Elem *tmpa = (Elem *)((((long)tmpv)+31L)&(-32L));
      // Someday we might need this alternate form.
      Elem *tmpa = (Elem *)(tmpv+31 - (((long)(tmpv+31))&(31L)));
      memcpy((void *)tmpa, (void *)array, sizeof(Elem)*arraySize);
  
      if (allocSize) delete[] varray;
      varray = tmpv;
      array = tmpa;
      allocSize = size;
    }

    // Empty function for now.
    // eventually, this should get smaller storage and free
    void reduce(void) {}; 

  public:
    friend class ResizeArray<Elem>;
    friend class SortableResizeArray<Elem>;
    friend class ResizeArrayIter<Elem>;

    inline int size(void) const { return arraySize; }
    inline Elem &operator[](int index) const { return array[index]; }

    // Default constructor 
    ResizeArrayRaw(void) : 
      array((Elem *)0), varray((unsigned char *)0), arraySize(0), allocSize(0) { 
      growthFactor = GrowthFactor;
      minSize = MinSize;
    }

    // Copy constructor - true copy on construction.
    ResizeArrayRaw(const ResizeArrayRaw<Elem> &rar ) : 
      array((Elem *)0), varray((unsigned char *)0), arraySize(0), allocSize(0) {
      growthFactor = rar.growthFactor;
      minSize = rar.minSize;
      // We want rar.size() slots, but no constructor run on the elements
      resizeRaw(rar.size());
      memcpy((void*)array, (void*)rar.array, sizeof(Elem)*rar.size());
      arraySize = rar.size();
    }
  
    // Encap a pre-existing array
    ResizeArrayRaw( Elem * * const array, int arraySize, int allocSize) {
      if (allocSize < arraySize) allocSize = arraySize;
      this->allocSize = allocSize;
      this->arraySize = arraySize;
      varray = (unsigned char *)*array;
      this->array = (Elem *)*array;
      *array = 0;
      growthFactor = GrowthFactor;
      minSize = MinSize;
    }
  
    ~ResizeArrayRaw(void) {
      for (int i=0; i < size(); i++) {
        array[i].~Elem();
      }
      delete[] varray;
    }
  
    // minSize = minimum growth size - (also initial size of array)
    // growthFactor = mulplicative factor by which to grow array.
    void setResizeParams(int min, float growth) {
      minSize = min;
      growthFactor = growth;
    }
  
  
    // True copy made on assignment.
    ResizeArrayRaw<Elem> & operator=(const ResizeArrayRaw<Elem> &rar ) {
      growthFactor = rar.growthFactor;
      minSize = rar.minSize;
  
      // Clean up this array
      resize(0);
      resizeRaw(rar.size());
  
      memcpy((void*)array, (void*)rar.array, sizeof(Elem)*rar.size());
      arraySize = rar.size();
      return *this;
    }
  
    // Properly constructs default object on new elements
    // Properly destructs on removed elements
    // arraySize is properly updated
    void resize(int size) {
      int i;
  
      if (size < arraySize) {
        for (i=size; i<arraySize; i++) {
          array[i].~Elem();
        }
      } else if (size > arraySize) {
        resizeRaw(size);
        for (i=arraySize; i<size; i++) {
          new ((void *)&array[i]) Elem;
        }
      }
      arraySize = size;
    }
  
    inline int del(int index, int number) {
      int i;
  
      // Fix up number to delete if deletes off end of array
      if (index >= arraySize)
        number=0; // for inline sake, don't have multiple returns
      else if (index+number-1 > arraySize) {
        number = index-arraySize+1;
      }
  
      // Destruct objects to be deleted
      for (i=index; i < index+number; i++) {
        array[i].~Elem();
      }
  
      // Shift down
      memmove((void *)(array+index),
         (void *)(array+index+number),
         (arraySize-number-index)*sizeof(Elem));
      
      // fixup size of array
      arraySize -= number;
      return(number);
    }
  
    
    // Insert element in array
    // If index is over the end of array, default
    // constructor should be run over blank elements to pad.
    inline void ins(const Elem &e, int index) {
      // Size array depending if index is in current array or reaches beyond.
      if (index < arraySize) {
        resizeRaw(arraySize+1);
        // Shift up
        memmove((void *)(array+index+1),
          (void *)(array+index),
          (arraySize-index)*sizeof(Elem));
      } else {
        resizeRaw(index+1);
      }
      
      // Write in new element via assignment - allows any refcounting
      // etc. to take place correctly!
      new((void *)&array[index]) Elem;
      array[index] = e;
    
      // Take care of fill and setting correct arraySize 
      if (index > arraySize) {
        for (Elem *tmp = array+arraySize; tmp < array+index; tmp++) {
          new ((void *)tmp) Elem;
        }
        arraySize = index+1;
      } else
        arraySize++;
    }
};	// end template definition

#endif
