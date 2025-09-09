#!/bin/python3
import numpy as np

def calculate_regression_coefficients(filepath, polyord):
    data = np.genfromtxt(filepath, delimiter = ',', skip_header = 1)
    
    pressure = data[:, 0]
    properties = [data[:, 1], data[:, 2], data[:, 3]]
    
    #calculate X matrix
    x_mat = np.zeros((len(pressure), polyord + 1))
    for i in range(polyord + 1):
        x_mat[:, i] = pressure**i

    x_trans = np.transpose(x_mat) #x transpose
    xtx = np.matmul(x_trans, x_mat) #x'x
    xtx_inv = np.linalg.inv(xtx) #(x'x)^-1
    coeffs = np.zeros((3, polyord + 1), dtype = float)    
    for i, prop in enumerate(properties):
        xty = np.matmul(x_trans, prop)
        coeff = np.matmul(xtx_inv, xty)
        coeffs[i, :] = coeff
        
    return coeffs


    

filePath = input("Enter the filename of the data: ")
polynomial_order = int(input("Enter the order of the polynomial to fit: "))

coefficients = calculate_regression_coefficients(filePath, polynomial_order)
headers = ["Density", "Viscosity", "Enthalpy"]

for i, header in enumerate(headers):
    print(f"{header}:")
    for j in range(polynomial_order + 1):
        print(f"{coefficients[i][j]}")
    print("\n")

