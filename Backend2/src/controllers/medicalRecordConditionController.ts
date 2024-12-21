import {Inject, Service} from "typedi";
import config from "../../config";
import IMedicalRecordConditionService from "../services/IServices/IMedicalRecordConditionService";
import IMedicalRecordConditionController from "./IControllers/IMedicalRecordConditionController";


@Service()
export default class MedicalRecordConditionController implements IMedicalRecordConditionController {

    constructor(
        @Inject(config.services.medicalRecordMedicalCondition.name) private medicalRecordMedicalConditionService: IMedicalRecordConditionService) { }
    
    
    public async getMedicalRecordConditions(req: any, res: any): Promise<void> {
        
        let medicalRecordId = req.query.recordNumberId;
        
        console.log("AQUIIII - " + medicalRecordId);
        
        await this.medicalRecordMedicalConditionService.getMedicalRecordConditions(medicalRecordId);
        
    }
    
    
}