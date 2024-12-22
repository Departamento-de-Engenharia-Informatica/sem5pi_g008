import {Inject, Service} from "typedi";
import IMedicalConditionController from "./IControllers/IMedicalConditionController";
import config from "../../config";
import IMedicalConditionDTO from "../dto/IMedicalConditionDTO";
import IMedicalConditionService from "../services/IServices/IMedicalConditionService";
import {AppError} from "../domain/MedicalCondition/Exceptions/AppError";

@Service()
export default class MedicalConditionController implements IMedicalConditionController {
    constructor(
        @Inject(config.services.medicalCondition.name) private medicalConditionServiceInstance: IMedicalConditionService
    ) {
    }


    public async createMedicalCondition(req: any, res: any) {

        let medicalConditionDTO: IMedicalConditionDTO = req.body;

        try {

            await this.medicalConditionServiceInstance.createMedicalCondition(medicalConditionDTO);

            res.status(200).json({
                message: 'Medical Condition created successfully'
            });

        } catch (error) {

            if(error instanceof AppError) {

                if(error.code >= 800 && error.code <= 804) {
                    console.log("Error: " + error.code + " - " + error.message);
                } else {
                    res.status(error.code).json({
                        message: error.message
                    });
                }

            } else {
                res.status(500).json({
                    message: 'Error creating medical condition - Code/Designation already exists',
                });
            }
        }
    }

    public async getAllMedicalConditions(req: any, res: any) {
        try {
            const medicalConditions = await this.medicalConditionServiceInstance.getAllMedicalConditions();
            res.status(200).json({
                medicalConditions: medicalConditions
            });
        } catch (error) {
            res.status(500).json({
                message: 'Error fetching medical conditions' + error,
            });
        }
    }
}
