//
// Created by rmclaren on 6/30/21.
//

#pragma once

#include "FortranObject.h"

#include <iostream>
#include <memory>
#include <string>
#include <vector>


namespace bufr
{
    struct ResultBase {
        std::string field_name;
        std::string group_by_field_name;
        std::vector<std::size_t> dims;
        std::vector<std::string> dimPaths;

        virtual ~ResultBase() {}
        virtual void print() = 0;
    };

    template <typename T>
    struct Result : ResultBase
    {
        typedef T value_type;
        
        std::vector<T> data;

        void print() final
        {
            std::cout << data.size() << std::endl;
            for (auto val : data)
            {
                std::cout << val << ", ";
            }
        }
    };

    class ResultSet : public FortranObject
    {
     public:
        ResultSet();
        ~ResultSet();

        std::shared_ptr<ResultBase> get(const std::string& field_name, 
                                        const std::string& group_by_field = "") const;

     protected:
        Address get_v_ptr() override;
    };
}  // namespace bufr
