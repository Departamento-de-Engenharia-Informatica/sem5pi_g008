import {Inject, Service} from "typedi";
import IMedicalRecordController from "./IControllers/IMedicalRecordController";
import config from "../../config";
import IMedicalRecordService from "../services/IServices/IMedicalRecordService";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";


@Service()
export default class MedicalRecordController implements IMedicalRecordController {

  constructor(
    @Inject(config.services.medicalRecord.name) private medicalRecordInstance: IMedicalRecordService
  ) {
  }

  public async createMedicalRecord(req: any, res: any): Promise<void> {

    let medicalRecordId = req.body.recordNumberId;

    try {
      await this.medicalRecordInstance.createMedicalRecord(medicalRecordId);
      res.status(201).json({
        message: 'Medical record created successfully',
      });
    } catch (error) {
      if (error.code === 11000) {
        console.error('Error creating medical record:', error.message);
        res.status(409).json({
          message: 'Error creating medical record - Duplicate entry',
        });
      } else {
        console.error('Error creating medical record:', error.message);
        res.status(500).json({
          message: 'Error creating medical record',
          details: error.message
        });
      }
    }
  }

  public async getAllergies(req: any, res: any): Promise<IMedicalRecordAllergyDTO[]> {
    let medicalRecordId = req.params.id;

    try {
      res.status(200).json({
        body: await this.medicalRecordInstance.getAllergies(medicalRecordId)
      });
    } catch (error) {
      console.error('Error getting allergies:', error.message);
      res.status(500).json({
        message: 'Error getting allergies',
        details: error.message
      });
    }

    return null;
  }

  public async updateMedicalRecordConditions(req: any, res: any): Promise<void> {
    const medicalRecordId = req.body.medicalRecordID;
    const updatedConditions = req.body.recordConditions;
    console.log("Updating medical conditions for medical record with ID CONTROLLER:", medicalRecordId);
    console.log("Updated conditions:", updatedConditions);

    try {
      await this.medicalRecordInstance.updateMedicalConditions(medicalRecordId, updatedConditions);
      res.status(200).json({
        message: 'Medical conditions updated successfully',
      });
    } catch (error: any) {
      console.error('Error updating medical conditions:', {
        message: error.message,
        stack: error.stack,
      });  

      res.status(500).json({
        message: 'Error updating medical conditions',
        details: error.message,
      });
    }
  }

}
