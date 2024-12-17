import {Inject, Service} from "typedi";
import IMedicalConditionController from "./IControllers/IMedicalConditionController";
import config from "../../config";
import IMedicalConditionDTO from "../dto/IMedicalConditionDTO";
import IMedicalConditionService from "../services/IServices/IMedicalConditionService";
import {AppError} from "../domain/MedicalCondition/Exceptions/AppError";

const errorStatusMapping: { [key: number]: number } = {
    801: 400, // Bad Request para CODE_NULL_UNDEFINED
    802: 400, // Bad Request para DESCRIPTION_NULL_UNDEFINED
    803: 400, // Bad Request para DESIGNATION_NULL_UNDEFINED
    804: 400, // Bad Request para CODE_INVALID_WHITESPACE
    805: 400, // Unprocessable Entity para INVALID_ICD11_CODE
    806: 400, // Bad Request para DESIGNATION_INVALID_WHITESPACE
    807: 400, // Bad Request para DESIGNATION_INVALID_LENGTH
    808: 400, // Bad Request para DESCRIPTION_INVALID_WHITESPACE
    809: 400, // Bad Request para DESCRIPTION_INVALID_LENGTH
};

@Service()
export default class MedicalConditionController implements IMedicalConditionController {
    constructor(
        @Inject(config.services.medicalCondition.name) private medicalConditionServiceInstance: IMedicalConditionService
    ) {
    }

    
    public async createMedicalCondition(req: any, res: any) {

        let medicalCondition: IMedicalConditionDTO = req.body;

        try {


            await this.medicalConditionServiceInstance.createMedicalCondition(medicalCondition);

            res.status(200).json({
                message: 'Medical Condition created successfully'
            });

        } catch (error) {

            if(error instanceof AppError) {
                const statusCode = errorStatusMapping[error.code];
                
                res.status(statusCode).json({
                    code: error.code,
                    message: error.message
                });
                
            } else {
                res.status(500).json({
                    message: 'An error occurred while creating the Medical Condition:' + error.message
                });
            }
        }
    }
}