//
// Created by rmclaren on 6/30/21.
//


#include "ResultSet.h"
#include "query_interface.h"

#include <algorithm> 
#include <iostream>
#include <sstream>


namespace bufr
{
    ResultSet::ResultSet()
    {
        Address data_ptr;
        result_set__allocate_f(&data_ptr);
        initialize(data_ptr, true);
    }

    ResultSet::~ResultSet()
    {
        if (is_owned_)
        {
            result_set__deallocate_f(data_ptr_);
        }
    }

    std::shared_ptr<ResultBase> ResultSet::get(const std::string& field_name,
                                               const std::string& group_by_field) const
    {
        double* data_ptr = nullptr;
        int* dims_ptr = nullptr;
        char* dims_paths_ptr = nullptr;
        int num_dims = 0;
        int dim_paths_str_len = 0;
        result_set__get_raw_f(class_data_ptr_,
                              field_name.c_str(),
                              group_by_field.c_str(),
                              &data_ptr,
                              &dims_ptr,
                              &num_dims,
                              &dims_paths_ptr,
                              &dim_paths_str_len);

        bool isString = result_set__is_string_f(class_data_ptr_, field_name.c_str());
        std::shared_ptr<ResultBase> result;

        if (isString)
        {
            auto data = std::vector<std::string>();

            const char* char_ptr = (char*) data_ptr;
            for (int row_idx = 0; row_idx < dims_ptr[0]; row_idx++)
            {
                std::string str = std::string(char_ptr + row_idx * sizeof(double), sizeof(double));

                // trim trailing whitespace from str
                str.erase(std::find_if(str.rbegin(), str.rend(),
                                       [](char c) { return !std::isspace(c); }).base(), str.end());

                data.push_back(str);
            }

            auto strResult = std::make_shared<Result<std::string>>();
            strResult->field_name = field_name;
            strResult->group_by_field_name = group_by_field;
            strResult->data = data;
            strResult->dims.push_back(dims_ptr[0]);
            result = strResult;
        }
        else
        {
            // Compute product of dimensions
            int tot_elements = 1;
            for (int row = 0; row < num_dims; row++)
            {
                tot_elements *= dims_ptr[row];
            }

            auto floatResult = std::make_shared<Result<float>>();
            floatResult->field_name = field_name;
            floatResult->group_by_field_name = group_by_field;
            floatResult->data = std::vector<float>(data_ptr, data_ptr + tot_elements);
            floatResult->dims = std::vector<std::size_t>(dims_ptr, dims_ptr + num_dims);
            result = floatResult;
        }

        // Add dim path strings
        const char* ws = " \t\n\r\f\v";
        for (int dim_idx = 0; dim_idx < num_dims; dim_idx++)
        {
            auto path_str = std::string(dims_paths_ptr + dim_idx * dim_paths_str_len, dim_paths_str_len);

            // Trim extra chars from the path str
            path_str.erase(path_str.find_last_not_of(ws) + 1);
            result->dimPaths.push_back(path_str);
        }

        return result;
    }

    Address ResultSet::get_v_ptr()
    {
        return &__modq_result_set_MOD___vtab_modq_result_set_Resultset;
    }
}
