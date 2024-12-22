import {Inject, Service} from "typedi";
import IMedicalRecordController from "./IControllers/IMedicalRecordController";
import config from "../../config";
import IMedicalRecordService from "../services/IServices/IMedicalRecordService";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";
import IMedicalRecordFreeTextDTO from "../dto/IMedicalRecordFreeTextDTO";


@Service()
export default class MedicalRecordController implements IMedicalRecordController {

  constructor(
    @Inject(config.services.medicalRecord.name) private medicalRecordInstance: IMedicalRecordService,
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

  public async addFreeText(req: any, res: any): Promise<any> {
    let freeText: IMedicalRecordFreeTextDTO = req.body;

    try {
      await this.medicalRecordInstance.addFreeText(freeText);

      res.status(200).json({
        message: 'Comment Added Successfully'
      });

    } catch (error) {
      console.error('Error adding comment:', error.message);

    }
  }
}
