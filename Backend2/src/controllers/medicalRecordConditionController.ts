import {Inject, Service} from "typedi";
import config from "../../config";
import IMedicalRecordConditionService from "../services/IServices/IMedicalRecordConditionService";
import IMedicalRecordConditionController from "./IControllers/IMedicalRecordConditionController";


@Service()
export default class MedicalRecordConditionController implements IMedicalRecordConditionController {

    constructor(
        @Inject(config.services.medicalRecordCondition.name) private medicalRecordMedicalConditionService: IMedicalRecordConditionService) { }
    
    
    public async getMedicalRecordConditions(req: any, res: any): Promise<void> {
        try {
            const recordNumberId = req.query.recordNumberId;

            const medicalConditionDTOList = await this.medicalRecordMedicalConditionService.getMedicalRecordConditions(recordNumberId);

            res.status(200).json({
                medicalRecordConditions: medicalConditionDTOList
            });
            
        } catch (error) {
            console.error('Error getting medical record conditions:', error.message);

            res.status(500).json({
                message: 'Error getting medical record conditions',

            });
        }
    }
    
    
}