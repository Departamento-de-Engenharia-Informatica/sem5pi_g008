import {Inject, Service} from "typedi";
import IAllergyController from "./IControllers/IAllergyController";
import config from "../../config";
import IAllergyService from "../services/IServices/IAllergyService";
import IAllergyDTO from "../dto/IAllergyDTO";
import {Allergy} from "../domain/Allergy/Allergy";

@Service()
export default class AllergyController implements IAllergyController {
    constructor(
        @Inject(config.services.allergy.name) private allergyServiceInstance: IAllergyService
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
