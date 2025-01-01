import {Inject, Service} from "typedi";
import IAllergyController from "./IControllers/IAllergyController";
import config from "../../config";
import IAllergyService from "../services/IServices/IAllergyService";
import IAllergyDTO from "../dto/IAllergyDTO";
import {NotFoundException} from "../Exceptions/NotFoundException";
import {AppError} from "../domain/Shared/Exceptions/AppError";

@Service()
export default class AllergyController implements IAllergyController {
    constructor(
        @Inject("AllergyService") private allergyServiceInstance: IAllergyService
    ) {
    }

    public async createAllergy(req: any, res: any): Promise<any> {

        let allergy: IAllergyDTO = req.body;

        try {
            await this.allergyServiceInstance.createAllergy(allergy);

            res.status(200).json({
                message: 'Allergy created successfully'
            });

        } catch (error) {
            console.error('Error creating allergy:', error.message);

            res.status(500).json({
                message: 'Error creating allergy - Duplicate entry',
            });
        }
    }
    
    public async updateAllergyDesignation(req: any, res: any) {
        let id = req.params.id;
        let designation = req.body.designation;
        
        try {
            let allergy = await this.allergyServiceInstance.updateAllergyDesignation(id, designation);
            res.status(200).json({
                allergy: allergy
            });
        } catch (error) {
            
            console.error('Error:' + error.message);
            
            if (error instanceof NotFoundException) {
                res.status(404).json({
                    message: error.message
                });
            } else if(error instanceof AppError) {
                res.status(error.code).json({
                    message: error.message
                });
            } else {
                res.status(500).json({
                    message: 'Error updating allergy:' + error,
                });
            }
        }
    }

    public async updateAllergyDescription(req: any, res: any) {
        let id = req.params.id;
        let description = req.body.description;

        try {
            let allergy = await this.allergyServiceInstance.updateAllergyDescription(id, description);
            res.status(200).json({
                allergy: allergy
            });
        } catch (error) {

            console.error('Error:' + error.message);

            if (error instanceof NotFoundException) {
                res.status(404).json({
                    message: error.message
                });
            } else if(error instanceof AppError) {
                res.status(error.code).json({
                    message: error.message
                });
            } else {
                res.status(500).json({
                    message: 'Error updating allergy:' + error,
                });
            }
        }
    }

    public async updateAllergyEffects(req: any, res: any) {
        let id = req.params.id;
        let effects = req.body.effects;

        try {
            
            await this.allergyServiceInstance.updateAllergyEffects(id, effects);
            res.status(200).json({
                message: 'Medical Condition symptoms updated successfully'
            });
        } catch (error) {
            console.log("Error: " + error.message);
            if (error instanceof NotFoundException) {
                res.status(404).json({
                    message: error.message
                });
            } else {
                res.status(500).json({
                    message: 'Error updating allergy:' + error,
                });
            }
        }
    }
    
    public async getAllAllergies(req: any, res: any): Promise<any> {
        try {
            let allergies = await this.allergyServiceInstance.getAllAllergies();

            res.status(200).json({
                allergies: allergies
            });

        } catch (error) {
            console.error('Error getting allergies:', error.message);

            res.status(500).json({
                message: 'Error getting allergies',
            });
        }
    }

    public async searchAllergies( req: any, res: any, next: any): Promise<any> {

        try {
            const allergies = await this.allergyServiceInstance.searchAllergies(req.params.filter);
            res.status(200).json({
                allergies: allergies
            });

        } catch (error) {
            console.error('Error getting allergies:', error.message);

            res.status(500).json({
                message: 'Error getting allergies'
            });
        }

    }

}
