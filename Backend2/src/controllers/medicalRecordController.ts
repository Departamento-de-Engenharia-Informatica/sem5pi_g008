import {Inject, Service} from "typedi";
import IMedicalRecordController from "./IControllers/IMedicalRecordController";
import config from "../../config";
import IMedicalRecordService from "../services/IServices/IMedicalRecordService";
import IMedicalRecordDTO from "../dto/IMedicalRecordDTO";


@Service()
export default class MedicalRecordController implements IMedicalRecordController {

    constructor(
        @Inject(config.controllers.medicalRecord.name) private medicalRecordInstance: IMedicalRecordService
    ) {}

   
    public async updateMedicalRecord(req: any, res: any): Promise<any> {

        let medicalRecord: IMedicalRecordDTO = req.body;

        try {
            await this.medicalRecordInstance.updateMedicalRecord(medicalRecord);
            res.status(200).json({
                message: 'Medical record updated successfully'
            });
        } catch (error) {
            console.error('Error updating medical record:', error.message);

        }
    }
}