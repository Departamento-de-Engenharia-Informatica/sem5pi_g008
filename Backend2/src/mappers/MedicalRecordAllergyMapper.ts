import {Mapper} from "../core/infra/Mapper";
import mongoose, {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {MedicalRecordFreeText} from "../domain/MedicalRecordFreeText/MedicalRecordFreeText";
import {MedicalRecordAllergy} from "../domain/MedicalRecordAllergy/MedicalRecordAllergy";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";
import {AllergyMap} from "./AllergyMap";
import {IMedicalRecordAllergyPersistence} from "../dataschema/IMedicalRecordAllergyPersistence";
import {Inject} from "typedi";
import config from "../../config";
import IMedicalRecordService from "../services/IServices/IMedicalRecordService";
import IAllergyService from "../services/IServices/IAllergyService";
import { Allergy } from "../domain/Allergy/Allergy";

export class MedicalRecordAllergyMapper extends Mapper<MedicalRecordAllergy> {


  public static async toDTO(domain: MedicalRecordAllergy): Promise<IMedicalRecordAllergyDTO> {

    return {
      allergy: domain.allergy,
      medicalRecordId: domain.domainId.toString(),
      doctor: domain.props.doctorId,
      comment: domain.props.comment,
    } as IMedicalRecordAllergyDTO;
  }

  public static toDomain (medicalRecordAllergy: any | Model<IMedicalRecordAllergyPersistence & Document> ): MedicalRecordAllergy {

    const medicalRecordAllergyResult = MedicalRecordAllergy.create(
      medicalRecordAllergy,
      new UniqueEntityID(medicalRecordAllergy.domainId)
    );

    medicalRecordAllergyResult.isFailure ? console.log(medicalRecordAllergyResult.error) : '';

    return medicalRecordAllergyResult.isSuccess ? medicalRecordAllergyResult.getValue() : null;
  }

  public static toPersistence(medicalRecordAllergy: MedicalRecordAllergy, id?: number): IMedicalRecordAllergyPersistence {

    return {
      domainId: id !== undefined ? id : medicalRecordAllergy.domainId,
      allergyId: new mongoose.Types.ObjectId(medicalRecordAllergy.allergy),
      medicalRecordId: new mongoose.Types.ObjectId(medicalRecordAllergy.medicalRecord),
      comment: medicalRecordAllergy.comment,
      doctorId: medicalRecordAllergy.doctorId,
    };
  }

}
