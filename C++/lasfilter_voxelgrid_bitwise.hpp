#ifndef VOXEL_GRID_H
#define VOXEL_GRID_H

#include <math.h>
// #include <set>
#include <stdio.h>
#include <vector>

#include "lasfilter.hpp"
#include "laszip_decompress_selective_v3.hpp"

/*class voxelGrid{

  public:
    void setVoxel(double vLen);
    int getLength(double first, double last);
    bool checkRegistry(double x, double y, double z);
    void resetDynamicReg();

  private:
    double voxelSideLength;
    std::set< std::vector<int> > dynamic_registry;
};*/

using namespace std;

typedef unsigned long long int LLU;
typedef std::vector<LLU> VLLU;
typedef std::vector<VLLU> VVLLU;
typedef std::vector<VVLLU> VVVLLU;

class voxelGrid{

  public:
    void setVoxel(double vLen)
    {
        voxelSpacing = vLen < 0 ? vLen : -vLen;
    };

    void setBitSize()
    {
        bitContainerSize = sizeof(LLU) * 8;
    };

    unsigned int getLength(double first, double last)
    {
        double distance = last - first;

        if(distance < 0)
            distance = -distance;

        unsigned int n = floor(distance / voxelSpacing);
        return n;
    };

    unsigned int getLength(double first, double last, unsigned short int& remainder)
    {
        double distance = last - first;

        if(distance < 0)
            distance = -distance;

        unsigned int n = floor(distance / voxelSpacing);

        remainder = n % bitContainerSize;
        n = ceil(n/bitContainerSize);

        return n;
    };

    bool checkRegistry(double x, double y, double z)
    {
        if(voxelSpacing < 0){
            xAnker = x;
            yAnker = y;
            zAnker = z;
            voxelSpacing = -voxelSpacing;
        }

        VVVLLU* registry;

        if(x < xAnker){
            if(y < yAnker){
                if(z < zAnker){
                    registry = &reg_xyz_minus;
                }else{
                    registry = &reg_z_plus_xy_minus;
                }
            }else{
                if(z < zAnker){
                    registry = &reg_y_plus_xz_minus;
                }else{
                    registry = &reg_yz_plus_x_minus;
                }
            }
        }else{
            if(y < yAnker){
                if(z < zAnker){
                    registry = &reg_x_plus_yz_minus;
                }else{
                    registry = &reg_xz_plus_y_minus;
                }
            }else{
                if(z < zAnker){
                    registry = &reg_xy_plus_z_minus;
                }else{
                    registry = &reg_xyz_plus;
                }
            }
        }

        unsigned short int zBit;

        unsigned int xPos = getLength(xAnker, x);
        unsigned int yPos = getLength(yAnker, y);
        unsigned int zPos = getLength(zAnker, z, zBit);

        if( registry->size() <= xPos )
        {
            VLLU zTemp( 0, 0 );
            VVLLU yTemp( 0, zTemp);
            VVVLLU xTemp( xPos + 1 - registry->size(), yTemp );
            registry->insert( registry->end(), xTemp.begin(), xTemp.end() );
        }
        
        if( (*registry)[xPos].size() <= yPos )
        {
            VVLLU* temp = &(*registry)[xPos];
            VLLU zTemp( 0, 0 );
            VVLLU yTemp( yPos + 1 - temp->size(), zTemp);
            temp->insert( temp->end(), yTemp.begin(), yTemp.end() );
        }
        
        if((*registry)[xPos][yPos].size() <= zPos)
        {
            VLLU* temp = &(*registry)[xPos][yPos];
            VLLU zTemp( zPos + 1 - temp->size(), 0 );
            temp->insert( temp->end(), zTemp.begin(), zTemp.end() );
        }

        LLU* bitHost = &(*registry)[xPos][yPos][zPos];
        LLU bitPoint = (LLU)1 << zBit;
        LLU bitChecker = *bitHost & bitPoint;

        bool isFilled = bitChecker == bitPoint;

        if(!isFilled)
            *bitHost |= bitPoint;

        return isFilled;
    };

    void resetDynamicReg(){
      VVVLLU temp;
      reg_xyz_plus = temp;
      reg_xy_plus_z_minus = temp;
      reg_xz_plus_y_minus = temp;
      reg_yz_plus_x_minus = temp;
      reg_x_plus_yz_minus = temp;
      reg_y_plus_xz_minus = temp;
      reg_z_plus_xy_minus = temp;
      reg_xyz_minus = temp;
      voxelSpacing *= -1;
    };

  private:
    double voxelSpacing;
    double xAnker;
    double yAnker;
    double zAnker;
    unsigned int bitContainerSize;

    VVVLLU reg_xyz_plus;
    VVVLLU reg_xy_plus_z_minus;
    VVVLLU reg_xz_plus_y_minus;
    VVVLLU reg_yz_plus_x_minus;
    VVVLLU reg_x_plus_yz_minus;
    VVVLLU reg_y_plus_xz_minus;
    VVVLLU reg_z_plus_xy_minus;
    VVVLLU reg_xyz_minus;

};

class LAScriterionThinWithVoxel : public LAScriterion{
  public:
    inline const CHAR* name() const { return "thin_with_voxel"; };
    inline I32 get_command(CHAR* string) const { return sprintf(string, "-%s %g ", name(), resolution); };
    inline U32 get_decompress_selective() const { return LASZIP_DECOMPRESS_SELECTIVE_CHANNEL_RETURNS_XY | LASZIP_DECOMPRESS_SELECTIVE_Z; };
    inline BOOL filter(const LASpoint* point){
      return box.checkRegistry(point->get_x(), point->get_y(), point->get_z());
    };
    void reset(){
      box.resetDynamicReg();
    };
    LAScriterionThinWithVoxel(F32 voxel_resolution){
      box.setVoxel(voxel_resolution);
      box.setBitSize();
    };
    ~LAScriterionThinWithVoxel(){ reset(); };

  private:
    voxelGrid box;
    double resolution;
};

#endif //VOXEL_GRID_H