import {Inject, Service} from "typedi";
import IMedicalRecordController from "./IControllers/IMedicalRecordController";
import config from "../../config";
import IMedicalRecordService from "../services/IServices/IMedicalRecordService";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";
import {NoMedicalRecordConditionsException} from "../domain/MedicalRecordCondition/NoMedicalRecordConditionsException";
import {NoMedicalRecordException} from "../domain/MedicalRecord/NoMedicalRecordException";


@Service()
export default class MedicalRecordController implements IMedicalRecordController {

  constructor(
    @Inject(config.services.medicalRecord.name) private medicalRecordInstance: IMedicalRecordService,) {}

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

  public async getMedicalRecordConditions(req: any, res: any): Promise<void> {
    try {

      let medicalRecordId = req.params.id;

      console.log('medicalRecordId:', medicalRecordId);
      
      const medicalConditionDTOList = await this.medicalRecordInstance.getMedicalRecordConditions(medicalRecordId);

      res.status(200).json({
        medicalRecordConditions: medicalConditionDTOList
      });

    } catch (error) {

      if (error instanceof NoMedicalRecordConditionsException) {
        res.status(error.code).json({
          message: error.message
        });
        return;
      }

      if (error instanceof NoMedicalRecordException) {
        res.status(error.code).json({
          message: error.message
        });
        return;
      }      
      
      console.log(error);
      
      res.status(500).json({
        message: 'Error getting medical record conditions.',

      });
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
}
